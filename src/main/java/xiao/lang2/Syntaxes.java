package xiao.lang2;

import java.lang.Void;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Interp.Ctx;
import static xiao.lang2.JavaInterop.*;
import static xiao.lang2.Names.*;
import static xiao.lang2.Pattern.Finder;
import static xiao.lang2.Procedures.*;
import static xiao.lang2.Procedures.Void;
import static xiao.lang2.Procedures.MultiValues.values;
import static xiao.lang2.Reader.MemberAccessorExpansion.isJavaClassName;
import static xiao.lang2.Values.*;

/**
 * special forms
 * @author chuxiaofeng
 */
public interface Syntaxes {

    static void init(Interp interp, Env e) {
        // todo 逐步干掉 syntax_rules
        e.put(SYNTAX_RULES, new SyntaxRules(interp));

        // todo 程序外层包裹一层 (lambda () ...) 或者 (let-values () ...), 这样就能用 define-values 了
        // todo 用宏实现
        // e.g. (define (a) a)  --> (define-values (a) (lambda () a))
        // e.g. (define-values (a) (quote 1))
        e.put(DEFINE, new Define(interp));
        e.put(SET, new Set(interp));
        e.put(IF, new If(interp));

        Begin begin = new Begin(interp, false);
        e.put(BEGIN, begin);
        // e.put("seq", begin);
        e.put(BEGIN0, new Begin(interp, true));

        Lambda λ = new Lambda(interp);
        e.put(LAMBDA, λ);
        e.put(LAMBDA0, λ);

        e.put(CASE_LAMBDA, new CaseLambda(interp));

        // quote 会原封不动返回, quasiquote 会处理内部 unquote unquote-splicing
        e.put(QUOTE, new Quote());
        e.put(QUASIQUOTE, new Quasiquote(interp));

        e.put(DEFINE_VALUES, new DefineValues(interp));
        e.put(LET_VALUES, new LetValues(interp, false));
        e.put(LETREC_VALUES, new LetValues(interp, true));

        e.put(DOT, new MemberAccessor(interp));
        e.put(NEW, new New(interp));

        e.put("debugger", new Debugger(interp));
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class Debugger implements Syntax {
        final Interp interp;

        Debugger(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length == 1, "(debugger a)");
            interp.interp1(forms[0], E, arg -> {
                System.out.println("----------------------------------------------");
                System.out.println("debugger: " + forms[0]);
                System.out.println("result: " + arg);
                System.out.println("----------------------------------------------\n");
                K.apply(Void());
            });
        }

        @Override
        public String toString() {
            return "#<syntax:debugger>";
        }
    }

    class If implements Syntax {
        final Interp interp;

        If(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String sig = "(if test consequent alternate)";
            Finder r = match(sig, forms);
            Object test = r.get("test");
            Object consequent = r.get("consequent");
            Object alternate = r.get("alternate");

            interp.interp1(test, E, v -> {
                // 这里采用跟 scheme 一样的逻辑, 只有 #f 是 false
                // expect(v instanceof Boolean, "类型错误: (if BOOL_COND then else)");
                // boolean bool = (Boolean) v;
                boolean bool = !Boolean.FALSE.equals(v);
                if (bool) {
                    interp.interp1(consequent, E, K);
                } else {
                    interp.interp1(alternate, E, K);
                }
            });
        }

        @Override
        public String toString() {
            return "#<syntax:" + IF + ">";
        }
    }

    class Set implements Syntax {
        final Interp interp;

        Set(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = tryMatch("(set! id expr)", forms);
            if (r == null) {
                setField(forms, E, K);
            } else {
                setId(forms, E, K);
            }
        }

        void setId(Object[] forms, Env E, Ctx K) {
            String sig = "(set! id expr)";
            Finder r = match(sig, forms);
            Object id = r.get("id");
            Object expr = r.get("expr");

            expect(id instanceof Symbol, sig);
            interp.interp1(expr, E, val -> {
                Syntaxes.setId(((Symbol) id), val, E, K);
            });
        }

        void setField(Object[] forms, Env E, Ctx K) {
            // (set! (. instance-expr instance-field-name) expr)
            // (set! (. className static-field-name) expr)
            String sig = "(set! (. cls-or-ins id:field) expr)";
            Finder r = match(sig, new String[] { "." }, forms);
            Object clsOrIns = r.get("cls-or-ins");
            Symbol field = r.get("id:field");
            Object expr = r.get("expr");
            if (isJavaClassName(clsOrIns)) {
                Class<?> klass = Misc.classOf(((Symbol) clsOrIns).name);
                interp.interp1(expr, E, val -> {
                    CallSite.field(klass, field.name).field(null, val);
                    K.apply(Void.TYPE);
                });
            } else {
                interp.interp1(clsOrIns, E, ins -> {
                    interp.interp1(expr, E, val -> {
                        Class<?> cls = ins.getClass();
                        CallSite.field(cls, field.name).field(ins, val);
                        K.apply(Void.TYPE);
                    });
                });
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + SET + ">";
        }
    }

    class Begin implements Syntax {
        final Interp interp;
        final boolean zero;

        Begin(Interp interp, boolean zero) {
            this.interp = interp;
            this.zero = zero;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            // String sig = "(begin body ...+)";
            String sig = "(begin body ...)";
            Finder r = match(sig, forms);
            List<Object> body = r.get("body");
            begin(interp, body, E, K, zero);
        }

        @Override
        public String toString() {
            return "#<syntax:" + BEGIN + ">";
        }
    }

    static void begin(Interp interp, List<Object> body, Env E, Ctx K, boolean zero) {
        if (body.isEmpty()) {
            K.apply(Void());
        } else {
            interp.interpN(body.toArray(), E, xs -> {
                Object[] a = (Object[]) xs;
                K.apply(a[zero ? 0 : a.length - 1]);
            });
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    class Quote implements Syntax {

        Quote() { }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length == 1, "语法错误: (quote ONLY_ONE)");
            K.apply(quote(forms[0]));
        }

        Object quote(Object a) {
            if (a instanceof PList) {
                PList l = (PList) a;
                if (l.size() == 3) {
                    Object car = l.get(0);
                    Object cadr = l.get(1);
                    Object caddr = l.get(2);
                    if (cadr instanceof Symbol && ((Symbol) cadr).name.equals(DOT)) {
                        return cons(quote(car), quote(caddr));
                    }
                }
                return map(this::quote, l);
            } else if (a instanceof Pair) {
                return cons(quote(car(a)), quote(cdr(a)));
            } else {
                return a;
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + QUOTE + ">";
        }
    }

    class Quasiquote implements Syntax {
        final Interp interp;

        Quasiquote(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length == 1, "语法错误: (quasiquote ONLY_ONE)");
            interpQuasiQuote(forms[0], E, K);
        }

        void interpQuasiQuote(Object C, Env E, Ctx K) {
            if (C instanceof PList) {
                PList lst = ((PList) C);
                if (lst.size() > 0 && sym(UNQUOTE).equals(lst.get(0))) {
                    expect(lst.size() == 2, "参数个数错误: (unquote ONLY_ONE_NODE)");
                    interp.interp1(lst.get(1), E, K);
                } else if (lst.size() > 0 && sym(UNQUOTE_SPLICING).equals(lst.get(0))) {
                    expect(lst.size() == 2, "参数个数错误: (unquote-splicing ONLY_ONE_NODE)");
                    interp.interp1(lst.get(1), E, v -> {
                        expect(v instanceof PList, "参数类型错误: (unquote-splicing LIST)");
                        K.apply(new Splicing(v));
                    });
                } else if (lst.size() == 3
                        && lst.get(1) instanceof Symbol
                        && ((Symbol) lst.get(1)).name.equals(DOT)) {
                    Object car = lst.get(0);
                    Object caddr = lst.get(2);
                    interpQuasiQuote(car, E, x -> {
                        interpQuasiQuote(caddr, E, y -> {
                            K.apply(cons(x, y));
                        });
                    });
                } else {
                    interpQuasiQuoteList(lst, 0, E, xs -> {
                        K.apply(Splicing.splice((Object[]) xs));
                    });
                }
            } else {
                K.apply(C);
            }
        }

        void interpQuasiQuoteList(
                PList lst,
                int idx,
                Env E,
                Ctx K
        ) {
            int sz = lst.size();
            if (idx == sz) {
                K.apply(new Object[sz]);
            } else {
                Object C = lst.get(idx);
                interpQuasiQuote(C, E, it -> {
                    interpQuasiQuoteList(lst, idx + 1, E, xs -> {
                        ((Object[]) xs)[idx] = it;
                        K.apply(xs);
                    });
                });
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + QUASIQUOTE + ">";
        }

        private static class Splicing {
            final Object v;

            Splicing(Object v) {
                this.v = v;
            }

            static PList splice(Object[] arr) {
                List<Object> lst = new ArrayList<>();
                for (Object o : arr) {
                    if (o instanceof Splicing) {
                        Object v = ((Splicing) o).v;
                        if (v instanceof PList) {
                            lst.addAll(((PList) v));
                        } else {
                            lst.add(v);
                        }
                    } else {
                        lst.add(o);
                    }
                }
                return listColl(lst);
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    class DefineValues implements Syntax {
        final Interp interp;

        public DefineValues(Interp interp) {
            this.interp = interp;
        }

        @Override
        public String toString() {
            return "#<syntax:" + DEFINE_VALUES + ">";
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = match("(define-values (id ...+) expr)", forms);
            List<Symbol> ids = r.get("id");
            Object expr = r.get("expr");
            interp.interp1(expr, E, x -> {
                PList values = values(x);
                expect(values.size() == ids.size(), "arity mismatch");
                defines(ids, values, E, K);
            });
        }
    }

    class LetValues implements Syntax {
        final Interp interp;
        final boolean rec;

        public LetValues(Interp interp, boolean rec) {
            this.interp = interp;
            this.rec = rec;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = match("(_ ([(id ...) val] ...) body ...+)", forms);
            PList idss = r.get("id"); // List<List<Symbol>>
            PList vals = r.get("val");
            PList bodys = r.get("body");
            if (rec) {
                Env letE = E.derive();
                letrecValues(idss, vals, bodys, letE, K);
            } else {
//                Env letE = E;
//                letValues(idss, vals, bodys, E, letE, K);
                letValues1(idss, vals, bodys, E, K);
            }
        }

        // 1.2.3 顺序原因, case
        // false
        // (println
        //  (letrec ((x (call/cc list)))
        //    (println x)
        //    (if (pair? x)
        //      ((car x) (lambda () x))
        //      (pair? (x)))
        //  ))
        // true
        // (println
        //  (let ((x (call/cc list)))
        //    (if (pair? x)
        //      ((car x) (lambda () x))
        //      (pair? (x)))))

        void letValues(PList idss, PList vals, PList bodys, Env E, Env letE, Ctx K) {
            if (isNull(vals)) {
//                // 特殊处理: (let (没有 bind) 开新作用域)
//                if (E == letE) {
//                    begin(interp, bodys, E.derive(), K, false);
//                } else {
//                    begin(interp, bodys, letE, K, false);
//                }
                // 之所以注释掉是因为宏展开 expandBody 中, 如果 body 中有 define-values
                // 会把 body 翻译成 letrec 开新作用域, 所以这里不用派生
                begin(interp, bodys, letE, K, false);
            } else {
                //noinspection unchecked
                List<Symbol> ids = ((List<Symbol>) car(idss));
                Object val = car(vals);
                // 1. 先求值
                interp.interp1(val, E, x -> {
                    PList values = values(x);
                    expect(values.size() == ids.size(), "arity mismatch");
                    // 2. 开新环境(这里每次都新环境,是为了处理👆call/cc的 case)
                    // 每次开新环境实际是 let* 的语义
                    // (let-values ([(a) 1] [(a) 2]) )
                    // (let-values ([(a) 1] [(b) a]) )
                    // 上面 case 靠宏展开的作用域检查来避免
                    Env newLetE = letE.derive();
                    // 3. 再 define
                    defines(ids, values, newLetE, void1 -> {
                        letValues(((PList) cdr(idss)), ((PList) cdr(vals)), bodys, E, newLetE, K);
                    });
                });
            }
        }

        void letValues1(PList idss, PList vals, PList bodys, Env E, Ctx K) {
            interp.interpN(vals.toArray(), E, x -> {
                Env letE = E.derive();
                Object[] xs = (Object[]) x;
                for (int i = 0; i < xs.length; i++) {
                    PList values = values(xs[i]);
                    //noinspection unchecked
                    List<Symbol> ids = ((List<Symbol>) idss.get(i));
                    expect(values.size() == ids.size(), "arity mismatch");
                    defines(ids, values, letE);
                }
                begin(interp, bodys, letE.derive(), K, false);
            });
        }

        void letrecValues(PList idss, PList vals, PList bodys, Env letE, Ctx K) {
            if (isNull(vals)) {
                begin(interp, bodys, letE, K, false);
            } else {
                //noinspection unchecked
                List<Symbol> ids = ((List<Symbol>) car(idss));
                Object val = car(vals);
                    // 1. 先 define #f
                    for (Symbol id : ids) {
                        define(id, false, letE);
                    }
                // 2. 再求值
                interp.interp1(val, letE, x -> {
                    PList values = values(x);
                    expect(values.size() == ids.size(), "arity mismatch");
                    // 3. 再赋值
                    setIds(ids, values, letE, void1 -> {
                        letrecValues(((PList) cdr(idss)), ((PList) cdr(vals)), bodys, letE, K);
                    });
                });
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + LET_VALUES + ">";
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    // (define id expr)
    // (define (id formals ...) body ...+)  ==> (define id (lambda (formals ...) body ...)
    // (define (id . formal) body ...+) ==> (define id (lambda formal body ...)
    class Define implements Syntax {
        final Interp interp;

        Define(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String sig = "(define id expr) or (define (id formals ...) body ...+)";
            expect(forms.length >= 2, sig);
            if (forms[0] instanceof Symbol) {
                bindExpr(forms, E, K);
            } else if (forms[0] instanceof PList) {
                bindProcedure(forms, E, K);
            } else {
                throw new InterpError(sig);
            }

            // 这里应该是个性能热点, 不用 try 来判断了
            // String sig = "(define (id formals ...) body ...+)";
            // Finder r = tryMatch(sig, forms);
            // if (r == null) { } else { }
        }

        // (define id expr)
        void bindExpr(Object[] forms, Env E, Ctx K) {
            String signature = "(define id expr)";
            Finder r = match(signature, forms);
            Object id = r.get("id");
            expect(id instanceof Symbol, signature);
            Object expr = r.get("expr");
            interp.interp1(expr, E, val -> define(((Symbol) id), val, E, K));
        }

        // (define (id . formal) body ...+) ==> (define id (lambda formal body ...)
        // (define (id formals ...) body ...+)  ==> (define id (lambda (formals ...) body ...)
        void bindProcedure(Object[] forms, Env E, Ctx K) {
            String signature = "(define (id formals ...) body ...+)";
            Finder r = match(signature, forms);
            Object id = r.get("id");
            expect(id instanceof Symbol, signature);
            List<Object> formals = r.get("formals");
            List<Object> body = r.get("body");

            Closure c;
            boolean isRestId = formals.size() == 2 && formals.get(0).equals(sym(DOT));
            if (isRestId) {
                Object restId = formals.get(1);
                c = makeClosure(interp, restId, body, E, signature);
            } else {
                c = makeClosure(interp, listColl(formals), body, E, signature);
            }
            define(((Symbol) id), c, E, K);
        }

        @Override
        public String toString() {
            return "#<syntax:" + DEFINE + ">";
        }
    }

    static void define(Symbol id, Object val, Env E) {
        String name = id.name;
        expect(E.lookupLocal(name) == null, "重复定义: " + name);
        E.put(name, val);
    }

    static void define(Symbol id, Object val, Env E, Ctx K) {
        define(id, val, E);
        K.apply(Void());
    }

    static void defines(List<Symbol> ids, List<Object> vals, Env E) {
        expect(ids.size() == vals.size());
        for (int i = 0; i < ids.size(); i++) {
            define(ids.get(i), vals.get(i), E);
        }
    }

    static void defines(List<Symbol> ids, List<Object> vals, Env E, Ctx K) {
        defines(ids, vals, E);
        K.apply(Void());
    }

//    static void defines1(PList ids, PList values, Env E, Ctx K) {
//        if (isNull(ids)) {
//            K.apply(Void());
//        } else {
//            define(((Symbol) car(ids)), car(values), E, void1 -> {
//                defines1(((PList) cdr(ids)), ((PList) cdr(values)), E, K);
//            });
//        }
//    }

    static void setId(Symbol id, Object val, Env E) {
        String name = id.name;
        Env definedScope = E.findDefinedScope(name);
        expect(definedScope != null, id + " 未定义");
        definedScope.put(name, val);
    }

    static void setId(Symbol id, Object val, Env E, Ctx K) {
        setId(id, val, E);
        K.apply(Void());
    }

    static void setIds(List<Symbol> ids, List<Object> vals, Env E, Ctx K) {
        expect(ids.size() == vals.size());
        for (int i = 0; i < ids.size(); i++) {
            setId(ids.get(i), vals.get(i), E);
        }
        K.apply(Void());
    }

//    static void setIds1(PList ids, PList values, Env E, Ctx K) {
//        if (isNull(ids)) {
//            K.apply(Void());
//        } else {
//            setId(((Symbol) car(ids)), car(values), E, void1 -> {
//                setIds1(((PList) cdr(ids)), ((PList) cdr(values)), E, K);
//            });
//        }
//    }

    // 注意: body 多表达式原来是在 stdlib 用宏支持的
    // 现在这里添加 begin, 也需要 stdlib 的 begin 来支持
    static Closure makeClosure(Interp interp, Object formals, List<Object> body, Env E, String msg) {
        if (formals instanceof PList) {
            List<Object> formal = ((PList) formals);
            int sz = formal.size();
            List<Symbol> params = new ArrayList<>(sz);

            boolean var = false;
            for (int i = 0; i < sz; i++) {
                Object n = formal.get(i);
                expect(n instanceof Symbol, msg + ", 期望 Name, 实际 " + n);
                Symbol param = (Symbol) n;
                if (param.name.equals(DOT)) {
                    expect( !var && i != 0 && i == sz - 2, msg); // 必须倒数第二个
                    var = true;
                    continue;
                }
                if (!var) {
                    params.add(param);
                }
            }

            Symbol rest = var ? (Symbol) formal.get(sz - 1) : null;
            checkDup(params, rest);
            return new Closure(interp, E, params, rest, beginOf(body));
        } else if (formals instanceof Symbol) {
            // rest-id
            Symbol restId = (Symbol) formals;
            return new Closure(interp, E, new ArrayList<>(0), restId, beginOf(body));
        } else {
            throw new InterpError(msg);
        }
    }

    static void checkDup(List<Symbol> params, Symbol rest) {
        String msg = "参数重名";
        HashSet<Symbol> set = new HashSet<>(params);
        if (rest == null) {
            expect(set.size() == params.size(), msg);
        } else {
            set.add(rest);
            expect(set.size() == params.size() + 1, msg);
        }
    }

    // (λ formals body ...+)
    // (lambda formals body ...+)
    // formals	=	(arg ...)
    // 	 	   |	(arg ...+ . rest-id)
    // 	 	   |	rest-id
    // todo 可以用 case-lambda + letrec 实现 lambda
    // https://www.cs.utah.edu/plt/publications/scheme09-fb.pdf
    // todo 用 keyword 实现可选命名参数
    class Lambda implements Syntax {
        final Interp interp;

        Lambda(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String signature = "(lambda formals body ...+)";
            Finder r = match(signature, forms);
            Object formals = r.get("formals");
            List<Object> body = r.get("body");
            String msg = "语法错误: " + signature;
            K.apply(makeClosure(interp, formals, body, E, msg));
        }

        @Override
        public String toString() {
            return "#<syntax:" + LAMBDA + ">";
        }
    }

    // (case-lambda [formals body ...+] ...)
    // formals = (id ...)
    // 	 	| (id ...+ . rest-id)
    // 	 	| rest-id
    class CaseLambda implements Syntax {
        final Interp interp;

        CaseLambda(Interp interp) {
            this.interp = interp;
        }

        static class Case {
            // 负数表示至少, max 表示任意
            final static int rest = Integer.MAX_VALUE;
            final int argCnt;
            final Closure closure;
            Case(int argCnt, Closure closure) {
                this.argCnt = argCnt;
                this.closure = closure;
            }
        }

        static class CaseClosure implements Procedure {
            final Case[] cases;

            CaseClosure(Case[] cases) {
                this.cases = cases;
            }

            @Override
            public void call(Object[] args, Env E, Ctx K) {
                for (Case c : cases) {
                    int expect = c.argCnt;
                    int actual = args.length;
                    if (
                            expect == actual // 精确匹配数量
                            || (expect < 0 && actual >= -expect) // (a ...+ . rest) 至少
                            || expect == Case.rest // rest-id 不定
                    ) {
                        c.closure.call(args, E, K);
                        break;
                    }
                }
                throw new InterpError("arity mismatch");
            }

            public String toString() {
                return "#<procedure>";
            }
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String signature = "(case-lambda [formals body ...+] ...)";
            Finder r = match(signature, forms);
            List<Object> formalss = r.get("formals");
            List<List<Object>> bodys = r.get("body");
            int sz = formalss.size();

            Case[] cases = new Case[sz];
            for (int i = 0; i < sz; i++) {
                Object formals = formalss.get(i);
                List<Object> body = bodys.get(i);
                int argCnt = argCount(formals);
                Closure c = makeClosure(interp, formals, body, E, signature);
                // 按照语义匹配第一个, argCnt 相同默认覆盖好了
                cases[i] = new Case(argCnt, c);
            }
            K.apply(new CaseClosure(cases));
        }

        // 这里不用检查类型, Lambda.closure 会检查
        int argCount(Object formals) {
            if (formals instanceof Symbol) {
                return Case.rest;
            } else if (formals instanceof PList) {
                int cnt = 0;
                for (Object el : ((PList) formals)) {
                    if (el.equals(sym(DOT))) {
                        return -cnt; // 至少 cnt 个
                    } else {
                        cnt++;
                    }
                }
                return cnt;
            } else {
                return 0;
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + CASE_LAMBDA + ">";
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    // todo 用健康宏实现!!!
    class New implements Syntax {
        final Interp interp;

        New(Interp interp) {
            this.interp = interp;
        }

        // todo test
        @Override
        public void call(Object[] args, Env E, Ctx K) {
            Finder r = tryMatch("(new id:class args ...)", args);
            if (r == null) {
                newInterfaces(args, E, K);
            } else {
                newClass(args, E, K);
            }
        }

        void newClass(Object[] args, Env E, Ctx K) {
            Finder r = match("(new id:class args ...)", args);
            Symbol clsSym = r.get("id:class");
            List<Object> argForms = r.get("args");
            Class<?> cls = Misc.classOf(clsSym.name);
            if (cls.isInterface()) {
                expect(argForms.size() == 1, "arity mismatch");
                newInterfaces1(new Class<?>[] { cls }, argForms.get(0), E, K);
            } else {
                CallSite ctor = CallSite.constructor(cls);
                interp.interpN(argForms.toArray(), E, argVals -> {
                    K.apply(ctor.newInstance(((Object[]) argVals)));
                });
            }
        }

        // todo test ...
        void newInterfaces(Object[] args, Env E, Ctx K) {
            Finder r = match("(new (id:interfaces ...+) map-string-to-lambda-expr)", args);
            List<Symbol> ifaceSyms = r.get("id:interfaces");
            Class<?>[] ifaces = ifaceSyms.stream().map(it -> Misc.classOf(it.name)).toArray(Class[]::new);

            Object map = r.get("map-string-to-lambda-expr");
            newInterfaces1(ifaces, map, E, K);
        }

        void newInterfaces1(Class<?>[] ifaces, Object methodMap, Env E, Ctx K) {
            interp.interp1(methodMap, E, methods -> {
                expect(methods instanceof Map, "contract violation");
                ((Map<?, ?>) methods).forEach((k, v) -> {
                    expect(k instanceof String, "contract violation");
                    expect(v instanceof Procedure, "contract violation");
                });

                //noinspection unchecked
                K.apply(Experimental.proxy(ifaces, ((Map<String, Procedure>) methods), E));
            });
        }

        @Override
        public String toString() {
            return "#<syntax:" + NEW + ">";
        }
    }

    // member access operator
    class MemberAccessor implements Syntax {
        final Interp interp;

        MemberAccessor(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length >= 2, "DOT 语法错误");
            if (isJavaClassName(forms[0])) {
                // 静态成员访问
                String className = ((Symbol) forms[0]).name;
                JavaAccessor resolved = resolve(Misc.classOf(className), null, forms);
                resolved.access(interp, E, K);
            } else {
                // 实例成员访问
                interp.interp1(forms[0], E, instance -> {
                    expect(instance != null, "方法 receiver 为 null");
                    JavaAccessor resolved = resolve(instance.getClass(), instance, forms);
                    resolved.access(interp, E, K);
                });
            }
        }

        boolean hasPublicNoArgsMethod(Class<?> klass, String name) {
            Class<?> c = klass;
            while (c != null) {
                try {
                    Method method = c.getDeclaredMethod(name);
                    if (Modifier.isPublic(method.getModifiers())) {
                        return true;
                    }
                } catch (NoSuchMethodException ignored) { }
                c = c.getSuperclass();
            }
            return false;
        }

        JavaAccessor resolve(Class<?> klass, Object instance, Object[] forms) {
            /*
            clojure 语法
            第一个操作数是 JavaName 则带是静态成员访问, 否则是实例成员访问

            第二个操作数是 Symbol 且没有参数
            除非有无参公开方法, 否则是字段访问
            如果第二个操作数 Symbol 以-开头, 只会解析成字段访问

            如果第二个操作数是 PList, 则是方法调用, PList 第一个元素是方法名称的 Symbol, 也可以展开

            (. instance-expr member-symbol)
            (. ClassName-symbol member-symbol)
            (. instance-expr -field-symbol)
            (. instance-expr (method-symbol args ...)) or (. instance-expr method-symbol args ...)
            (. ClassName-symbol (method-symbol args ...)) or (. ClassName-symbol method-symbol args ...)
             */

            String msg = "DOT 语法错误";

            Object member;
            String methodName = null;
            String fieldName = null;
            Object[] argForms;
            boolean isMethod = forms[1] instanceof PList;
            if (isMethod) {
                expect(forms.length == 2, msg);
                PList lst = (PList) forms[1];
                expect(lst.size() >= 1, msg);
                member = car(lst);
                expect(member instanceof Symbol, msg);
                argForms = ((PList) cdr(lst)).toArray();
                methodName = ((Symbol) member).name;
            } else {
                member = forms[1];
                argForms = Arrays.copyOfRange(forms, 2, forms.length);
                expect(member instanceof Symbol, msg);
                // 强制属性访问
                boolean startWithHyphen = ((Symbol) member).name.startsWith(HYPHEN);
                // java 方法属性名不可能是 -前缀,不需要先判断hyphen
                if (argForms.length > 0) {
                    expect(!startWithHyphen, msg);
                    isMethod = true;
                    methodName = ((Symbol) member).name;
                } else {
                    if(startWithHyphen) {
                        fieldName = ((Symbol) member).name.substring(1);
                    } else {
                        String name = ((Symbol) member).name;
                        if (hasPublicNoArgsMethod(klass, name)) {
                            isMethod = true;
                            methodName = name;
                        } else {
                            fieldName = name;
                        }
                    }
                }
            }

            // 这里不需要处理构造函数, 在 expand 中展开成(new )
            if (isMethod) {
                if (instance == null) {
                    // 静态方法调用
                    return new CallStaticMethod(klass, methodName, argForms);
                } else {
                    // 实例方法调用
                    return new CallInstanceMethod(instance, methodName, argForms);
                }
            } else  {
                if (instance == null) {
                    // 静态字段访问
                    return new AccessStaticField(klass, fieldName);
                } else {
                    // 实例字段访问
                    return new AccessInstanceField(instance, fieldName);
                }
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + DOT + ">";
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    class SyntacticClosure implements Syntax {
        final Interp interp;
        final Macro macro;

        SyntacticClosure(Interp interp, Macro macro) {
            this.interp = interp;
            this.macro = macro;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Macro.Expansion expansion = macro.matchAndExpand(forms, E);

            // 这里貌似不能用 derive 作用域, e.g. 假设宏内部有 define,
            // define的变量就不在原来的作用域, 脑补成展开代码的语义即可
            Object expr = Reader.read(expansion.expression);

            Env callingScope = expansion.callingScope/*==E*/;
            interp.interp1(expr, callingScope, K);
        }

        @Override
        public String toString() {
            return "#<syntactic-closure>";
        }
    }

    class SyntaxRules implements Syntax {
        final Interp interp;

        SyntaxRules(Interp interp) {
            this.interp = interp;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String signature = "(syntax-rules (literal-id ...) [(id pattern ...) template] ...)";
            Finder r = match(signature, forms);
            List<Symbol> formals = r.get("literal-id");
            List<List<Object>> patterns = r.get("pattern");
            List<Object> templates = r.get("template");

            List<Macro.Rule> rules = new ArrayList<>(patterns.size());
            for (int i = 0; i < patterns.size(); i++) {
                List<Object> pattern = patterns.get(i);
                Object template = templates.get(i);
                rules.add(new Macro.Rule(listColl(pattern), template));
            }

            Macro macro = new Macro(E, formals, rules);
            K.apply(new SyntacticClosure(interp, macro));
        }

        @Override
        public String toString() {
            return "#<syntax:" + SYNTAX_RULES + ">";
        }
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    static Finder match(String pattern, String[] formals, Object[] argForms) {
        PList input = list(
                sym("_"),
                splice(argForms)
        );
        return Pattern.ofStr(pattern, formals).match(input);
    }

    static Finder match(String pattern, Object[] argForms) {
        return match(pattern, new String[0], argForms);
    }

    static Finder tryMatch(String pattern, String[] formals, Object[] argForms) {
        try {
            return match(pattern, formals, argForms);
        } catch (InterpError e) {
            return null;
        }
    }

    static Finder tryMatch(String pattern, Object[] argForms) {
        return tryMatch(pattern, new String[0], argForms);
    }

    static Object beginOf(List<Object> forms) {
        if (forms.size() == 1) {
            return forms.get(0);
        } else {
            return list(sym(BEGIN), splice(forms));
        }
    }
}
