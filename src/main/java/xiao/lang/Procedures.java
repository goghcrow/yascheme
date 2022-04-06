package xiao.lang;

import xiao.lang.expander.CompiledExpression;
import xiao.lang.expander.Expander;
import xiao.lang.expander.Namespace;

import java.util.ArrayList;
import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Names.*;
import static xiao.lang.Values.*;

/**
 * @author chuxiaofeng
 */
@SuppressWarnings("unused")
public interface Procedures {

    static void init(Expander expander, Env e) {
        // todo 环境有问题
        e.put(EXPAND, new Eval(expander, true));
        e.put(EVAL, new Eval(expander));

        e.put(APPLY, new Apply());
        e.put(CALLCC, new CallCC());

        e.put(VALUES, new MultiValues.Values());
        e.put(CALL_WITH_VALUES, new MultiValues.CallWithValues());

        e.put(MATCH_SYNTAX, new MatchSyntax(false));
        e.put(TRY_MATCH_SYNTAX, new MatchSyntax(true));
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class Eval implements Values.Procedure {
        final Expander expander;
        final boolean expandOnly;

        Eval(Expander expander) {
            this(expander, false);
        }

        Eval(Expander expander, boolean expandOnly) {
            this.expander = expander;
            this.expandOnly = expandOnly;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            String name = expandOnly ? EXPAND : EVAL;
            String sig = "(" + name + " form env?)";
            expect(args.length == 1 || args.length == 2, "arity mismatch");
            Object C = args[0];
            Namespace ns = expander.currentNamespace();
            if (args.length == 2) {
                expect(args[1] instanceof Namespace, "contract violation");
                ns = ((Namespace) args[1]);
            }

            CompiledExpression compiled = expander.compileModule(Reader.read(C), ns);
            if (expandOnly) {
                K.apply(compiled.sexpr);
            } else {
                Object val = expander.eval(compiled);
                K.apply(val);
            }
        }

        @Override
        public String toString() {
            return "#<procedure:" + (expandOnly ? EXPAND : EVAL) + ">";
        }
    }

    class Apply implements Values.Procedure {
        @Override
        public void call(Object[] args, Env E, Ctx K) {
            String sig = "(apply proc list)";
            expect(args.length == 2, "参数个数错误: " + sig);
            expect(args[1] instanceof PList, "参数类型错误: " + sig);

            Object[] lstArgs = ((PList) args[1]).toArray();
            if (args[0] instanceof Procedure) {
                ((Procedure) args[0]).call(lstArgs, E, K);
            }
//            else if (args[0] instanceof Syntax) {
//                // 特殊处理支持 apply 一些自定义的宏
//                ((Syntax) args[0]).call(lstArgs, E, K);
//            }
            else {
                throw new InterpError("参数类型错误: " + sig);
            }
        }

        @Override
        public String toString() {
            return "#<procedure:" + APPLY + ">";
        }
    }

    class CallCC implements Values.Procedure {
        @Override
        public void call(Object[] args, Env E, Ctx K) {
            expect(args.length == 1, "语法错误: (callcc (λ (k) ...))");
            expect(args[0] instanceof Procedure, "参数类型错误: (callcc (λ (k) ...))");
            Procedure f = ((Procedure) args[0]);
            // 把控制流包装成 callable, 把控制流变成 value
            f.call(new Object[] { new Continuation(K) }, E, K);
        }

        @Override
        public String toString() {
            return "#<procedure:" + CALLCC + ">";
        }
    }

    class Closure implements Values.Procedure {
        public final Env env;
        public final List<Symbol> params;
        public @Nullable final Symbol rest;
        public final Object body;

        Closure(Env env, List<Symbol> params, @Nullable Symbol rest, Object body) {
            this.env = env;
            this.params = params;
            this.rest = rest;
            this.body = body;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            if (rest == null) {
                doCall(args, K);
            } else {
                doCallVarArgs(args, K);
            }
        }

        void doCall(Object[] args, Ctx K) {
            Env scope = env.derive();
            expect(args.length == params.size(),
                    "arity mismatch: excepted %d, actual %d", params.size(), args.length);
            for (int i = 0; i < params.size(); i++) {
                String param = params.get(i).name;
                scope.put(param, args[i]);
            }
            Interp.interp1(body, scope, K);
        }

        void doCallVarArgs(Object[] args, Ctx K) {
            Env scope = env.derive();
            int minSz = params.size();
            expect(args.length >= minSz,
                    "arity mismatch: expect >= %d, actual %d", minSz, args.length);

            List<Object> varArgs = new ArrayList<>(args.length - minSz);
            for (int i = 0; i < args.length; i++) {
                if (i < minSz) {
                    scope.put(params.get(i).name, args[i]);
                } else {
                    varArgs.add(args[i]);
                }
            }
            scope.put(rest.name, RT.listColl(varArgs));

            Interp.interp1(body, scope, K);
        }

        @Override
        public String toString() {
            return "#<procedure>";
        }
    }

    class Continuation implements Values.Procedure {
        final Ctx K;

        Continuation(Ctx k) {
            K = k;
        }

        @Override
        public void call(Object[] args, Env E_, Ctx K_) {
            expect(args.length == 1, "参数个数错误: (callcc (λ (k) ...))");
            K.apply(args[0]);
        }

        @Override
        public String toString() {
            return "#<continuation>";
        }
    }

    interface MultiValues {
        Object magic = RT.cons(RT.sym("multiple"), RT.sym("values"));

        static boolean isMagic(Object x) {
            return RT.isPair(x) && RT.car(x) == magic;
        }

        static PList values(Object x) {
            if (isMagic(x)) {
                return ((PList) RT.cdr(x));
            } else {
                return RT.list(x);
            }
        }

        class Values implements Procedure {
            @Override
            public void call(Object[] args, Env E, Ctx K) {
                if (args.length == 1) {
                    K.apply(args[0]);
                } else {
                    K.apply(RT.cons(magic, RT.list(args)));
                }
            }

            @Override
            public String toString() {
                return "#<procedure:" + VALUES + ">";
            }
        }

        // (call-with-values generator receiver) → any
        class CallWithValues implements Procedure {
            @Override
            public void call(Object[] args, Env E, Ctx K) {
                expect(args.length == 2, "arity mismatch");
                expect(args[0] instanceof Procedure, "contract violation");
                expect(args[1] instanceof Procedure, "contract violation");
                Procedure producer = ((Procedure) args[0]);
                Procedure consumer = ((Procedure) args[1]);
                producer.call(new Object[0], E, x -> {
                    consumer.call(values(x).toArray(), E, K);
                });
            }

            @Override
            public String toString() {
                return "#<procedure:" + CALL_WITH_VALUES + ">";
            }
        }
    }

    class MatchSyntax implements Values.Procedure {
        final boolean tryMatch;

        MatchSyntax(boolean tryMatch) {
            this.tryMatch = tryMatch;
        }

        // try 返回 #f
        // ([try-]syntax-match stx pattern formals) -> procedure|#f
        // stx: syntax
        // pattern: any
        // formals: list

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            expect(args.length == 2 || args.length == 3, "arity mismatch");
            String[] formals = formals(args);
            Object stx = args[0];
            Object pattern = args[1];
            Pattern ptn = Pattern.of(pattern, formals);
            Pattern.Finder r;
            if (tryMatch) {
                r = ptn.tryMatch(stx);
                if (r == null) {
                    K.apply(false);
                    return;
                }
            } else {
                r = ptn.match(stx);
            }
            K.apply(Procedure.nameOf(MATCH_SYNTAX + "-get-result", (args1, E1, K1) -> {
                expect(args1.length == 1, "arity mismatch");
                expect(args1[0] instanceof Symbol, "contract violation");
                K1.apply(r.get(((Symbol) args1[0]).name));
            }));
        }

        String[] formals(Object[] args) {
            if (args.length == 3) {
                expect(args[2] instanceof PList);
                PList fs = (PList) args[2];
                String[] formals = new String[fs.size()];
                for (int i = 0; i < fs.size(); i++) {
                    expect(fs.get(i) instanceof Symbol, "contract violation");
                    formals[i] = ((Symbol) fs.get(i)).name;
                }
                return formals;
            } else {
                return new String[0];
            }
        }

        public String toString() {
            return "#<procedure:" + (tryMatch ? TRY_MATCH_SYNTAX : MATCH_SYNTAX) + ">";
        }
    }
}
