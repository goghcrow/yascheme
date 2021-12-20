package xiao.lang;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.resource;
import static xiao.lang.Procedures.*;
import static xiao.lang.Values.*;

/**
 * CEK Interp
 * @author chuxiaofeng
 */
public class Interp {

    public static void interp(String s) {
        Interp interp = new Interp();
        Env env = interp.newScope();
        interp.interp(Reader.read(s), env, System.out::println);
    }

    public static void run(Runnable r) {
        Trampoline.run(r);
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    final Env globalScope;

    final static String[] stdLibs = {
            "/core.ss",
            "/interop.ss",
            "/condition.ss",
            "/symbol.ss",
            "/string.ss",
            "/coerce.ss",
            "/pair-list.ss",
            "/math.ss",
            "/multi-values.ss",
            "/do-cond-case.ss",
            "/print.ss",
            "/assert.ss",
    };

    public Interp() {
        this(true);
    }

    public Interp(boolean init) {
        if (init) {
            Env env = new Env();

            init(this, env);
            Syntaxes.init(this, env);

            // 这里为了一些 primitive 可以被 override
            env = env.derive();
            for (String stdLib : stdLibs) {
                String s = resource(stdLib);
                interp(Reader.read(s), env, v -> { });
            }
            globalScope = env;
        } else {
            globalScope = new Env();
        }
    }


//    /////////////////////////////////////////////
//    // for 新的健康宏展开
//    public Interp() {
//        this(true);
//    }
//
//    public Interp(boolean init) {
//        if (init) {
//            throw new IllegalStateException();
//        } else {
//            globalScope = new Env();
//        }
//    }
//    /////////////////////////////////////////////



    public void interp(Object C, Env E, Ctx K) {
        Trampoline.run(() -> interp1(C, E, K));
    }

    public Env newScope() {
        return globalScope.derive();
    }

    // INTERNAL USAGE ONLY
    // 这里不用模式匹配来处理一些 syntax 的好处是, 没有什么特殊的 form
    // lambda define ... 都是 runtime 的 value ==> Syntax
    // 坏处就是解释器实现起来不像模式匹配看起来优雅
    public void interp1(Object C, Env E, Ctx K) {
        // System.out.println(C); // for debug
        if (C instanceof PList) {
            PList lst = ((PList) C);
            expect(lst.size() != 0, "语法错误");
            // todo 宏展开已经处理, 这里可以干掉
            // 处理 java交互语法 special form, todo 放在 reader 中
            Object expanded = Reader.MemberAccessorExpansion.expandMemberAccessor(C);
            if (expanded == C) {
                Object callee = car(lst);
                Object[] args = ((PList) cdr(lst)).toArray();
                interpCall(callee, args, E, K);
            } else {
                interp1(expanded, E, K);
            }
        } else if (C instanceof Symbol) {
            String id = ((Symbol) C).name;
            Object val = E.lookup(id);
            K.apply(val);
        } else if (C instanceof SyntacticClosure) {
            SyntacticClosure sc = (SyntacticClosure) C;
            interp1(sc.C, sc.E, K);
        } else {
            // literal
            K.apply(C);
        }
    }

//    // for 新的健康宏展开
//    public void interp1(Object C, Env E, Ctx K) {
//        // System.out.println(C); // for debug
//        if (C instanceof PList) {
//            PList lst = ((PList) C);
//            expect(lst.size() != 0, "语法错误");
//            Object callee = car(lst);
//            Object[] args = ((PList) cdr(lst)).toArray();
//            interpCall(callee, args, E, K);
//        } else if (C instanceof Symbol) {
//            String id = ((Symbol) C).name;
//            Object val = E.lookup(id);
//            K.apply(val);
//        } else {
//            K.apply(C);
//        }
//    }

    void interpCall(Object calleeForm, Object[] argForms, Env E, Ctx K) {
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

    void interpN(Object[] N, Env E, Ctx K) {
        interpN(N, 0, E, K);
    }

    private void interpN(Object[] N, int idx, Env E, Ctx K) {
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

        void doApply(Object v);
    }

    // 尾调用优化
    // 也可以用标准的 CEK 机器来实现, 这样就不用用蹦床来实现尾调用优化了
    static class Trampoline {
        // 如果只用一个实例大概 10% 性能提升
        static class KThrow extends Error {
            final Ctx K;
            final Object v;

            KThrow(Ctx k, Object v) {
                super(null, null, true, false);
                K = k;
                this.v = v;
            }
        }

        static void Return(Ctx k, Object v) {
            throw new KThrow(k, v);
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
