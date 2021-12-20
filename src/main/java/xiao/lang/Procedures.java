package xiao.lang;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Misc.sneakyThrows;
import static xiao.lang.Names.*;
import static xiao.lang.Values.*;

/**
 * @author chuxiaofeng
 */
@SuppressWarnings("unused")
public interface Procedures {

    static void init(Interp interp, Env e) {
        e.put(EVAL, new Eval(interp));
        e.put(APPLY, new Apply(interp));
        e.put(CALLCC, new CallCC(interp));
        e.put(VALUES, new MultiValues.Values());
        e.put(CALL_WITH_VALUES, new MultiValues.CallWithValues());
        e.put(MATCH_SYNTAX, new MatchSyntax(false));
        e.put(TRY_MATCH_SYNTAX, new MatchSyntax(true));
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class Eval implements Values.Procedure {
        final Interp interp;

        public Eval(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            String sig = "(eval form env?)";
            expect(args.length == 1, "arity mismatch");
            Object C = args[0];
            interp.interp1(C, E, K);
        }

        @Override
        public String toString() {
            return "#<procedure:" + EVAL + ">";
        }
    }

    class Apply implements Values.Procedure {
        final Interp interp;

        public Apply(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            String sig = "(apply proc list)";
            expect(args.length == 2, "参数个数错误: " + sig);
            expect(args[1] instanceof PList, "参数类型错误: " + sig);

            Object[] lstArgs = ((PList) args[1]).toArray();
            if (args[0] instanceof Procedure) {
                ((Procedure) args[0]).call(lstArgs, E, K);
            } else if (args[0] instanceof Syntax) {
                // 特殊处理支持 apply 一些自定义的宏
                ((Syntax) args[0]).call(lstArgs, E, K);
            } else {
                throw new InterpError("参数类型错误: " + sig);
            }
        }

        @Override
        public String toString() {
            return "#<procedure:" + APPLY + ">";
        }
    }

    class CallCC implements Values.Procedure {
        final Interp interp;

        public CallCC(Interp interp) {
            this.interp = interp;
        }

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
        final Interp interp;
        public final Env env;
        public final List<Symbol> params;
        public @Nullable final Symbol rest;
        public final Object body;

        Closure(Interp interp,
                Env env,
                List<Symbol> params,
                @Nullable Symbol rest,
                Object body) {
            this.interp = interp;
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
            interp.interp1(body, scope, K);
        }

        void doCallVarArgs(Object[] args, Ctx K) {
            Env scope = env.derive();
            int minSz = params.size();
            expect(args.length >= minSz,
                    "arity mismatch: expect >= %d, actual %d", minSz, args.length);

            PList varArgs = listCap(args.length - minSz);
            for (int i = 0; i < args.length; i++) {
                if (i < minSz) {
                    scope.put(params.get(i).name, args[i]);
                } else {
                    varArgs.add(args[i]);
                }
            }
            scope.put(rest.name, varArgs);

            interp.interp1(body, scope, K);
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
            return "#<procedure>";
        }
    }

    interface MultiValues {
        Object magic = cons(sym("multiple"), sym("values"));

        static boolean isMagic(Object x) {
            return isPair(x) && car(x) == magic;
        }

        static PList values(Object x) {
            if (isMagic(x)) {
                return ((PList) cdr(x));
            } else {
                return list(x);
            }
        }

        class Values implements Procedure {
            @Override
            public void call(Object[] args, Env E, Ctx K) {
                if (args.length == 1) {
                    K.apply(args[0]);
                } else {
                    K.apply(cons(magic, list(args)));
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
            Pattern ptn = Pattern.of2(pattern, formals);
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
            K.apply(new NamedProcedure(MATCH_SYNTAX + "-result", (args1, E1, K1) -> {
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

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    static void throw1(Throwable t) {
        sneakyThrows(t);
    }

    static boolean eq(Object a, Object b) {
        return a == b;
    }

    // ------------------------------------------------------------

    // todo remove
    static PList listCap(int sz) {
        return new PList(sz);
    }

    static PList listColl(Collection<Object> c) {
        if (c instanceof UnmodifiablePList) {
            return ((UnmodifiablePList) c);
        } else if (c.isEmpty()) {
            return Null();
        } else {
            return new UnmodifiablePList(c);
        }
    }

    static PList list(Object... args) {
        if (args.length == 0) {
            return Null();
        } else {
            return new UnmodifiablePList(quasilist(args));
        }
    }

    class Splicing {
        final Collection<?> c;
        Splicing(Collection<?> c) { this.c = c; }
    }

    static Splicing splice(Collection<?> c) {
        return new Splicing(c);
    }

    static Splicing splice(Object[] arr) {
        return new Splicing(Arrays.asList(arr));
    }

    static PList quasilist(Object... args) {
        PList lst = new PList();
        for (Object it : args) {
            if (it instanceof Splicing) {
                lst.addAll(((Splicing) it).c);
            } else {
                lst.add(it);
            }
        }
        return lst;
    }

    static Keyword keyword(String name) {
        return Keyword.of(name);
    }

    static Symbol sym(String name) {
        return Symbol.of(name, true);
    }

    // ------------------------------------------------------------

    static boolean isKeyword(Object o) {
        return o instanceof Keyword;
    }

    static boolean isSymbol(Object o) {
        return o instanceof Symbol;
    }

    static boolean isPair(Object o) {
        return o instanceof Pair || (o instanceof PList && !isNull(o));
    }

    static boolean isList(Object o) {
        return o instanceof PList;
    }

    static boolean isNull(Object v) {
        return Null().equals(v);
    }

    // ------------------------------------------------------------

    static Object Void() {
        return Void.TYPE;
    }
    static PList Null() {
        return UnmodifiablePList.NULL;
    }

    static Object car(Object o) {
        expect(isPair(o), "类型错误: (car PAIR)");
        if (o instanceof Pair) {
            return ((Pair) o).car;
        } else {
            expect(!isNull(o), "不能 car null");
            return ((PList) o).get(0);
        }
    }

    static Object cadr(Object o) {
        return car(cdr(o));
    }

    static Object cdr(Object o) {
        expect(isPair(o), "类型错误: (car PAIR)");
        if (o instanceof Pair) {
            return ((Pair) o).cdr;
        } else {
            expect(!isNull(o), "不能 cdr null");
            return ((PList) o).subList(1, ((PList) o).size());
            // todo
            // return listColl(((PList) o).subList(1, ((PList) o).size()));
        }
    }

    static PList cons(Object car, PList cdr) {
        return list(car, splice(cdr));
    }

    static Object cons(Object car, Object cdr) {
        if (cdr instanceof PList) {
            return cons(car, ((PList) cdr));
        } else {
            return new Pair(car, cdr);
        }
    }

    // ------------------------------------------------------------

    static PList map(Function<Object, Object> mapper, PList lst) {
        PList r = listCap(lst.size());
        for (Object o : lst) {
            r.add(mapper.apply(o));
        }
        return r;
    }

    static PList map(BiFunction<Object, Object, Object> mapper, PList l1, PList l2) {
        int sz = l1.size();
        expect(sz == l2.size());
        PList r = listCap(sz);
        for (int i = 0; i < sz; i++) {
            r.add(mapper.apply(l1.get(i), l2.get(i)));
        }
        return r;
    }

    interface TriFunction<T1, T2, T3, R> {
        R apply(T1 t1, T2 t2, T3 t3);
    }

    static PList map(TriFunction<Object, Object, Object, Object> mapper, PList l1, PList l2, PList l3) {
        int sz = l1.size();
        expect(sz == l2.size() && sz == l3.size());
        PList r = listCap(sz);
        for (int i = 0; i < sz; i++) {
            r.add(mapper.apply(l1.get(i), l2.get(i), l3.get(i)));
        }
        return r;
    }

    static PList map(Function<PList, Object> mapper, PList ...lst) {
        expect(lst.length > 0);
        int sz = lst[0].size();
        for (PList l : lst) {
            expect(sz == l.size());
        }
        PList r = listCap(sz);
        for (int i = 0; i < sz; i++) {
            PList args = listCap(lst.length);
            for (PList l : lst) {
                args.add(l.get(i));
            }
            r.add(mapper.apply(args));
        }
        return r;
    }

    // ------------------------------------------------------------

    static PList append() {
        return Null();
    }

    static PList append(PList a) {
        return listColl(a);
    }

    static PList append(PList a, PList b) {
        if (isNull(a)) return listColl(b);
        if (isNull(b)) return listColl(a);
        return list(splice(a), splice(b));
    }

    static PList append(PList ...lst) {
        return list(Arrays.stream(lst).map(Procedures::splice).toArray());
    }

    // ------------------------------------------------------------

    static Symbol gensym() {
        return gensym("g");
    }

    static Symbol gensym(String prefix) {
        return Symbol.gen(prefix);
    }
}
