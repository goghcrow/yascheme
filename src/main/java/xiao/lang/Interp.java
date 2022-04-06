package xiao.lang;

import xiao.lang.expander.CompiledExpression;
import xiao.lang.expander.Expander;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.*;
import static xiao.lang.RT.car;
import static xiao.lang.RT.cdr;
import static xiao.lang.Values.*;

/**
 * Interp
 *
 * 用异常实现 Trampoline 优化尾调用
 * 非标准的 CEK 实现, 也可以用标准的 CEK 机器来实现, 这样就不用用蹦床来实现尾调用优化了
 *
 * @author chuxiaofeng
 */
public class Interp {

    public static Object read(String s) {
        return Reader.read(s);
    }

    public static Object read(Object form) {
        return Reader.read(form);
    }

    public static String pp(Object form) {
        return PrettyPrint.pp(form);
    }

    public static CompiledExpression compile(Path path) {
        return Expander.of().compile(path);
    }

    public static Object eval(Path path) {
        return compile(path).eval();
    }

    // without caching compiled result

    public static CompiledExpression compile(String s) {
        Expander expander = Expander.of();
        Object form = Reader.read(s);
        return expander.compileModule(form, expander.currentNamespace());
    }

    public static Object eval(String s) {
        return compile(s).eval();
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    public static Env bootEnv(Expander expander) {
        Env env = new Env();
        Procedures.init(expander, env);
        Syntaxes.init(env);
        interp(Reader.read(resource("/boot.ss")), env, v -> { });
        return env;
    }

    public static Env procedures(Expander expander) {
        Env env = new Env();
        bootEnv(expander).forEach((sym, v) -> {
            if (v instanceof Procedure) {
                // 环境中来自 ss (core.ss) 文件定义的 procedure 没有名字, 这里加上
                env.put(sym, Procedure.nameOf(sym, ((Procedure) v)));
            }
        });
        return env;
    }

    public static Env syntaxes() {
        Env env = new Env();
        Syntaxes.init(env);
        return env;
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    private Interp() { }

    public static void interp(Object C, Env E, Ctx K) {
        Trampoline.run(() -> interp1(C, E, K));
    }

    public static void interp1(Object C, Env E, Ctx K) {
        // System.out.println(C); // for debug
        if (C instanceof PList) {
            PList lst = ((PList) C);
            expect(lst.size() != 0, "语法错误");
            Object callee = car(lst);
            Object[] args = ((PList) cdr(lst)).toArray();
            interpCall(callee, args, E, K);
        } else if (C instanceof Symbol) {
            String id = ((Symbol) C).name;
            Object val = E.lookup(id);
            K.apply(val);
        } else {
            K.apply(C);
        }
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    static void interpCall(Object calleeForm, Object[] argForms, Env E, Ctx K) {
        interp1(calleeForm, E, callee -> {
            if (callee instanceof Syntax) {
                ((Syntax) callee).call(argForms, E, K);
            } else if (callee instanceof Procedure) {
                interpN(argForms, E, args -> {
                    ((Procedure) callee).call(((Object[]) args), E, K);
                });
            } else {
                throw new InterpError("错误: 期望 Syntax 或者 Callable, 实际, " + calleeForm);
            }
        });
    }

    static void interpN(Object[] N, Env E, Ctx K) {
        interpN(N, 0, E, K);
    }

    private static void interpN(Object[] N, int idx, Env E, Ctx K) {
        int sz = N.length;
        if (idx == sz) {
            K.apply(new Object[sz]);
        } else {
            Object C = N[idx];
            interp1(C, E, val -> {
                interpN(N, idx + 1, E, xs -> {
                    ((Object[]) xs)[idx] = val;
                    K.apply(xs);
                });
            });
        }
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    public interface Ctx {

        default void apply(Object v) {
            Trampoline.Return(this, v);
        }

        /*private*/ void doApply(Object v);

    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    public static void run(Runnable r) {
        Trampoline.run(r);
    }

    static class Trampoline {
        static class KThrow extends Error {
            /*final */Ctx K;
            /*final */Object v;

            private KThrow(Ctx k, Object v) {
                super(null, null, true, false);
                K = k;
                this.v = v;
            }

            final static KThrow ins = new KThrow(null, null);
            static KThrow of(Ctx K, Object v) {
                // 一个实例大概 10% 性能提升 todo 验证一下
                // throw new KThrow(K, v);

                ins.K = K;
                ins.v = v;
                return ins;
            }
        }

        static void Return(Ctx k, Object v) {
            throw KThrow.of(k, v);
        }

        static void run(Runnable f) {
            while (true) {
                try {
                    f.run();
                    break;
                } catch (KThrow k) {
                    f = () -> k.K.doApply(k.v);
                }
            }
        }
    }
}
