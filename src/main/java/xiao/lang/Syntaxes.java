package xiao.lang;

import java.lang.Void;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interop.*;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Names.*;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.Procedures.Closure;
import static xiao.lang.Procedures.MultiValues.values;
import static xiao.lang.RT.Void;
import static xiao.lang.RT.*;
import static xiao.lang.Reader.isJavaClassName;
import static xiao.lang.Values.*;

/**
 * special forms
 * @author chuxiaofeng
 */
public interface Syntaxes {

    static void init(Env e) {
        e.put(SET, new Set());
        e.put(IF, new If());

        e.put(BEGIN, new Begin(false)); // seq
        e.put(BEGIN0, new Begin(true));

        //noinspection NonAsciiCharacters
        Lambda λ = new Lambda();
        e.put(LAMBDA, λ);
        e.put(LAMBDA0, λ);
        e.put(CASE_LAMBDA, new CaseLambda());

        e.put(QUOTE, new Quote());

        e.put(DEFINE_VALUES, new DefineValues());
        e.put(LET_VALUES, new LetValues());
        e.put(LETREC_VALUES, new LetrecValues());

        e.put(DOT, new MemberAccessor());
        e.put(NEW, new New());

        e.put(DEBUGGER, new Debugger());
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class Debugger implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length <= 1, "(debugger a)");
            if (forms.length == 1) {
                // todo 用宏实现
                System.out.println("----------------------------------------------");
                System.out.println("debugger: " + forms[0]);
                Interp.interp1(forms[0], E, arg -> {
                    System.out.println("result: " + arg);
                    System.out.println("----------------------------------------------\n");
                    K.apply(Void());
                });
            } else {
                K.apply(Void());
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + DEBUGGER + ">";
        }
    }

    class If implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String sig = "(if test consequent alternate)";
            Finder r = match(sig, forms);
            Object test = r.get("test");
            Object consequent = r.get("consequent");
            Object alternate = r.get("alternate");

            Interp.interp1(test, E, v -> {
                // 这里采用跟 scheme 一样的逻辑, 只有 #f 是 false
                // expect(v instanceof Boolean, "类型错误: (if BOOL_COND then else)");
                // boolean bool = (Boolean) v;
                boolean bool = !Boolean.FALSE.equals(v);
                if (bool) {
                    Interp.interp1(consequent, E, K);
                } else {
                    Interp.interp1(alternate, E, K);
                }
            });
        }

        @Override
        public String toString() {
            return "#<syntax:" + IF + ">";
        }
    }

    class Set implements Syntax {
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
            Interp.interp1(expr, E, val -> {
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
                Interp.interp1(expr, E, val -> {
                    CallSite.field(klass, field.name).field(null, val);
                    K.apply(Void.TYPE);
                });
            } else {
                Interp.interp1(clsOrIns, E, ins -> {
                    Interp.interp1(expr, E, val -> {
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
        final boolean zero;

        Begin(boolean zero) {
            this.zero = zero;
        }

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            // String sig = "(begin body ...+)";
            String sig = "(begin body ...)";
            Finder r = match(sig, forms);
            List<Object> body = r.get("body");
            begin(body, E, K, zero);
        }

        @Override
        public String toString() {
            return "#<syntax:" + BEGIN + ">";
        }
    }

    static void begin(List<Object> body, Env E, Ctx K, boolean zero) {
        if (body.isEmpty()) {
            K.apply(Void());
        } else {
            Interp.interpN(body.toArray(), E, xs -> {
                Object[] a = (Object[]) xs;
                K.apply(a[zero ? 0 : a.length - 1]);
            });
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    class Quote implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length == 1, "语法错误: (quote ONLY_ONE)");
            K.apply(forms[0]);
        }

        @Override
        public String toString() {
            return "#<syntax:" + QUOTE + ">";
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    class DefineValues implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = match("(define-values (id ...+) expr)", forms);
            List<Symbol> ids = r.get("id");
            Object expr = r.get("expr");
            Interp.interp1(expr, E, x -> {
                PList values = values(x);
                expect(values.size() == ids.size(), "arity mismatch");
                defines(ids, values, E, K);
            });
        }

        @Override
        public String toString() {
            return "#<syntax:" + DEFINE_VALUES + ">";
        }
    }

    class LetValues implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = match("(let-values ([(id ...) val] ...) body ...+)", forms);
            PList idss = r.get("id"); // List<List<Symbol>>
            PList vals = r.get("val");
            PList bodys = r.get("body");
            letValues(idss, vals, bodys, E, K);
        }

        void letValues(PList idss, PList vals, PList bodys, Env E, Ctx K) {
            // 这里注意环境派生从 K 外部转移到 K 内部, 因为如果在同一个环境重复执行绑定会导致重复定义
            // 这里处理成, 在 val 求值完的 K 内部派生环境
            // 没有按照规范去实现, 表现出来的语义是相同的

            // 原因: https://www.zhihu.com/question/297207095/answer/509225101
            // "Another restriction is that the continuation of each <init> should not be invoked more than once."
            // 不是说在 let 的 <init> 位置用 call/cc 不对，而是说即使是用了，continuation 也是从绑定完成后才开始，
            // 而不是如一般的从 (call/cc ...) 出现的位置开始。是不该再多调用超过一次，而不是不该调用。
            // (assert-equals true
            //  (let ((x (call/cc list)))
            //    (if (pair? x)
            //      ;; 在求值完绑定右值之后在派生新环境
            //      ;; bind(x  to  (lambda () x in E1)  in  E2)
            //      ;; (pair? (x))  => true, x 是 旧环境 的 (list k)
            //      ;; letrec 则是一直在同一个环境, set! x 把 x 改成 (lambda ...) 了
            //      ((car x) (lambda () x))
            //      (pair? (x)))))
            //
            //(assert-equals false
            //  (letrec ((x (call/cc list)))
            //    (if (pair? x)
            //      ((car x) (lambda () x))
            //      (pair? (x)))
            //  ))

            // 所以, 逻辑上 K 内外执行都一样的逻辑, 因为 K 会被重复执行, 都会造成差异

            // Env letE = E.derive();
            Interp.interpN(vals.toArray(), E, x -> {
                Env letE = E.derive();
                Object[] xs = (Object[]) x;
                for (int i = 0; i < xs.length; i++) {
                    PList values = values(xs[i]);
                    //noinspection unchecked
                    List<Symbol> ids = ((List<Symbol>) idss.get(i));
                    expect(values.size() == ids.size(), "arity mismatch");
                    defines(ids, values, letE);
                }
                begin(bodys, letE.derive(), K, false);
            });
        }

        @Override
        public String toString() {
            return "#<syntax:" + LET_VALUES + ">";
        }
    }

    class LetrecValues implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            Finder r = match("(letrec-values ([(id ...) val] ...) body ...+)", forms);
            PList idss = r.get("id"); // List<List<Symbol>>
            PList vals = r.get("val");
            PList bodys = r.get("body");
            Env letE = E.derive();
            letrecValues(idss, vals, bodys, letE, K);
        }

        void letrecValues(PList idss, PList vals, PList bodys, Env letE, Ctx K) {
            if (RT.isNull(vals)) {
                begin(bodys, letE, K, false);
            } else {
                //noinspection unchecked
                List<Symbol> ids = ((List<Symbol>) RT.car(idss));
                Object val = RT.car(vals);
                // 1. 先 define #f
                for (Symbol id : ids) {
                    define(id, false, letE);
                }
                // 2. 再求值
                Interp.interp1(val, letE, x -> {
                    PList values = values(x);
                    expect(values.size() == ids.size(), "arity mismatch");
                    // 3. 再赋值
                    setIds(ids, values, letE, void1 -> {
                        letrecValues(((PList) RT.cdr(idss)), ((PList) RT.cdr(vals)), bodys, letE, K);
                    });
                });
            }
        }

        @Override
        public String toString() {
            return "#<syntax:" + LETREC_VALUES + ">";
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

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
    static Closure makeClosure(Object formals, List<Object> body, Env E, String msg) {
        if (formals instanceof PList) {
            List<Object> formal = ((PList) formals);
            int sz = formal.size();
            List<Symbol> params = new ArrayList<>(sz);

            for (Object n : formal) {
                expect(n instanceof Symbol, msg + ", 期望 Symbol, 实际 " + n);
                params.add((Symbol) n);
            }

            checkDup(params, null);
            return new Closure(E, params, null, beginOf(body));
        } else if (formals instanceof Pair) {
            List<Symbol> params = new ArrayList<>();
            Object d = formals;
            while (isPair(d)) {
                Object a = car(d);
                expect(a instanceof Symbol, msg + ", 期望 Symbol, 实际 " + a);
                params.add(((Symbol) a));
                d = cdr(d);
            }
            expect(d instanceof Symbol, msg + ", 期望 Symbol, 实际 " + d);
            Symbol rest = ((Symbol) d);

            checkDup(params, rest);
            return new Closure(E, params, rest, beginOf(body));
        } else if (formals instanceof Symbol) {
            // rest-id
            Symbol restId = (Symbol) formals;
            return new Closure(E, new ArrayList<>(0), restId, beginOf(body));
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
    class Lambda implements Syntax {
        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            String signature = "(lambda formals body ...+)";
            Finder r = match(signature, forms);
            Object formals = r.get("formals");
            List<Object> body = r.get("body");
            String msg = "语法错误: " + signature;
            K.apply(makeClosure(formals, body, E, msg));
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
                Closure c = makeClosure(formals, body, E, signature);
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
                return ((PList) formals).size();
            } else if (formals instanceof Pair) {
                int cnt = 0;
                Object d = formals;
                while (isPair(d)) {
                    cnt++;
                    d = cdr(d);
                }
                return -cnt;
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
        // todo test
        @Override
        public void call(Object[] args, Env E, Ctx K) {
//            Finder r = tryMatch("(new id:class args ...)", args);
//            if (r == null) {
//                newInterfaces(args, E, K);
//            } else {
                newClass(args, E, K);
//            }
        }

        void newClass(Object[] args, Env E, Ctx K) {
            Finder r = match("(new id:class args ...)", args);
            Symbol clsSym = r.get("id:class");
            List<Object> argForms = r.get("args");
            Class<?> cls = Misc.classOf(clsSym.name);
//            if (cls.isInterface()) {
//                expect(argForms.size() == 1, "arity mismatch");
//                newInterfaces1(new Class<?>[] { cls }, argForms.get(0), E, K);
//            } else {
                CallSite ctor = CallSite.constructor(cls);
                Interp.interpN(argForms.toArray(), E, argVals -> {
                    K.apply(ctor.newInstance(((Object[]) argVals)));
                });
//            }
        }

//        // todo test ...
//        void newInterfaces(Object[] args, Env E, Ctx K) {
//            Finder r = match("(new (id:interfaces ...+) map-string-to-lambda-expr)", args);
//            List<Symbol> ifaceSyms = r.get("id:interfaces");
//            Class<?>[] ifaces = ifaceSyms.stream().map(it -> Misc.classOf(it.name)).toArray(Class[]::new);
//
//            Object map = r.get("map-string-to-lambda-expr");
//            newInterfaces1(ifaces, map, E, K);
//        }
//
//        void newInterfaces1(Class<?>[] ifaces, Object methodMap, Env E, Ctx K) {
//            Interp.interp1(methodMap, E, methods -> {
//                expect(methods instanceof Map, "contract violation");
//                ((Map<?, ?>) methods).forEach((k, v) -> {
//                    expect(k instanceof String, "contract violation");
//                    expect(v instanceof Procedure, "contract violation");
//                });
//
//                //noinspection unchecked
//                K.apply(Experimental.proxy(ifaces, ((Map<String, Procedure>) methods), E));
//            });
//        }

        @Override
        public String toString() {
            return "#<syntax:" + NEW + ">";
        }
    }

    // member access operator
    class MemberAccessor implements Syntax {

        @Override
        public void call(Object[] forms, Env E, Ctx K) {
            expect(forms.length >= 2, "DOT 语法错误");
            if (isJavaClassName(forms[0])) {
                // 静态成员访问
                String className = ((Symbol) forms[0]).name;
                JavaAccessor resolved = resolve(Misc.classOf(className), null, forms);
                resolved.access(E, K);
            } else {
                // 实例成员访问
                Interp.interp1(forms[0], E, instance -> {
                    expect(instance != null, "方法 receiver 为 null");
                    JavaAccessor resolved = resolve(instance.getClass(), instance, forms);
                    resolved.access(E, K);
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

        // clojure 语法
        // 第一个操作数是 JavaName 则带是静态成员访问, 否则是实例成员访问
        //
        // 第二个操作数是 Symbol 且没有参数
        // 除非有无参公开方法, 否则是字段访问
        // 如果第二个操作数 Symbol 以-开头, 只会解析成字段访问
        //
        // 如果第二个操作数是 PList, 则是方法调用, PList 第一个元素是方法名称的 Symbol, 也可以展开
        //
        // (. instance-expr member-symbol)
        // (. ClassName-symbol member-symbol)
        // (. instance-expr -field-symbol)
        // (. instance-expr (method-symbol args ...)) or (. instance-expr method-symbol args ...)
        // (. ClassName-symbol (method-symbol args ...)) or (. ClassName-symbol method-symbol args ...)
        JavaAccessor resolve(Class<?> klass, Object instance, Object[] forms) {
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
                member = RT.car(lst);
                expect(member instanceof Symbol, msg);
                argForms = ((PList) RT.cdr(lst)).toArray();
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

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    static Finder match(String pattern, String[] formals, Object[] argForms) {
        PList input = RT.list(
                RT.sym("_"),
                RT.splice(argForms)
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

    static Object letValues(List<Object> forms) {
        return list(sym("let-values"), list(), forms);
    }

    static Object beginOf(List<Object> forms) {
        if (forms.size() == 1) {
            return forms.get(0);
        } else {
            return RT.list(RT.sym(BEGIN), RT.splice(forms));
        }
    }
}
