package xiao.lang2.expander2;

import xiao.lang2.Env;
import xiao.lang2.InterpError;
import xiao.lang2.Procedures;

import java.util.ArrayList;
import java.util.List;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Misc.Nullable;
import static xiao.lang2.Pattern.Finder;
import static xiao.lang2.Procedures.*;
import static xiao.lang2.Reader.MemberAccessorExpansion.isJavaClassName;
import static xiao.lang2.Values.PList;
import static xiao.lang2.Values.Symbol;
import static xiao.lang2.expander2.CUtils.each;
import static xiao.lang2.expander2.CUtils.map;
import static xiao.lang2.expander2.Expansion.*;
import static xiao.lang2.expander2.Utils.match;
import static xiao.lang2.expander2.Utils.tryMatch;

/**
 * expand-expr
 * @author chuxiaofeng
 */
public interface CoreForms {

    // Common expansion for `lambda` and `case-lambda`
    static PList makeLambdaExpander(@Nullable Syntax lambda,
                                    Syntax s,
                                    Syntax formals,
                                    List<Syntax> bodys,
                                    ExpandContext ctx) {
        // (lambda (x) (x)) expands to
        // ⇒ (lambda (x:sc) (x:sc))
        // ------------------------------------------------------------
        // e.g. (lambda (x) (f x))
        // x+sc, f+sc, x in body+sc
        // x+localBinding
        // ------------------------------------------------------------

        // local-scope
        // Fully Expanded Program 的 binding form （let-values、#%plain-lambda等）所引入的scope，
        // 用于区分local 的 binding. 另外，quote-syntax（不带#:local）完全展开时，会删去结果中的local scope.
        Scope sc = Scope.of();

        // Parse and check formal arguments:
        PList ids = parseAndFlattenFormals(formals, sc);
        DupCheck.checkNoDuplicateIds(ids, s);
        // Bind each argument and generate a corresponding key for the
        // expand-time environment:
        Env bodyEnv = ctx.env.derive();
        for (int i = 0; i < ids.size(); i++) {
            Syntax id = (Syntax) ids.get(i);
            if (Syntax.isDot(id)) { // 处理 a . rest
                expect( i != 0 && i == ids.size() - 2);
            } else {
                Symbol key = Bindings.addLocalBinding(id);
                bodyEnv.put(key.name, Bindings.variable);
            }
        }

        // Expand the function body:
        ExpandContext bodyCtx = ctx.newEnv(bodyEnv);

        // Return formals (with new scope) and expanded body:
        if (lambda == null) {
            return list(
                    Scope.add(formals, sc),
                    expandBody(bodys, sc, s, bodyCtx)
            );
        } else {
            return list(
                    lambda,
                    Scope.add(formals, sc),
                    expandBody(bodys, sc, s, bodyCtx)
            );
        }
    }

    static PList parseAndFlattenFormals(Syntax allFormals, Scope sc) {
        return parseAndFlattenFormals1(allFormals, allFormals, sc);
    }

    static PList parseAndFlattenFormals1(Syntax oriAllFormals, Object formals, Scope sc) {
        if (Syntax.isIdentifier(formals)) {
            return list(Scope.add(((Syntax) formals), sc));
        } else if (formals instanceof Syntax) {
            Object p = ((Syntax) formals).e;
            if (isPair(p)) {
                return parseAndFlattenFormals1(oriAllFormals, p, sc);
            } else if (isNull(p)) {
                return Null();
            } else {
                throw new InterpError("not an identifier: " + p);
            }
        } else if (isNull(formals)) {
            return Null();
        } else if (formals instanceof PList) {  // fast-route
            // isPair + isNull 两个路径可以替代 Plist
            return Procedures.map(id -> {
                expect(Syntax.isIdentifier(id), "not an identifier:" + id);
                return Scope.add(id, sc);
            }, ((PList) formals));
        } else if (isPair(formals)) {
            Object car = car(formals);
            Object cdr = cdr(formals);
            expect(Syntax.isIdentifier(car), "not an identifier:" + car);
            return cons(
                    Scope.add(car, sc),
                    parseAndFlattenFormals1(oriAllFormals, cdr, sc)
            );
        } else {
            throw new InterpError("bad argument sequence: " + oriAllFormals);
        }
    }

    static Syntax lambda(Syntax s, ExpandContext ctx) {
        Finder r = match("(lambda formals body ...+)", s);
        Syntax lambda = r.get("lambda");
        Syntax formals = r.get("formals");
        List<Syntax> bodys = r.get("body");
        PList form = makeLambdaExpander(lambda, s, formals, bodys, ctx);
        return rebuild(s, form);
    }

    static Syntax caseLambda(Syntax s, ExpandContext ctx) {
        Finder r = match("(case-lambda [formals body ...+] ...)", s);
        Finder cr = match("(case-lambda clause ...)", s);
        Syntax caseLambda = r.get("case-lambda");
        List<Syntax> formalss = r.get("formals");
        List<List<Syntax>> bodyss = r.get("body");
        List<Syntax> clauses = cr.get("clause");

        return rebuild(s, list(
                caseLambda,
                splice(map(formalss, bodyss, clauses, (formals, bodys, clause) -> {
                    PList lambda = makeLambdaExpander(null, s, formals, bodys, ctx);
                    return rebuild(clause, lambda);
                }))
        ));
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // Common expansion for `let[rec]-[syntaxes+]values`
    static SyntaxTransformer makeLetValuesForm(boolean syntaxes, boolean rec) { return (s, ctx) -> {
        // local-scope
        // Fully Expanded Program 的 binding form （let-values、#%plain-lambda等）所引入的scope，
        // 用于区分local的 binding. 另外，quote-syntax（不带#:local）完全展开时，会删去结果中的local scope.
        Scope sc = Scope.of();

        // Add the new scope to each binding identifier:
        Finder r;
        List<Syntax> transRhss;
        List<List<Syntax>> transIdss;

        if (syntaxes) {
            r = match("(letrec-syntaxes+values\n" +
                    "   ([(trans-id ...) trans-rhs] ...)\n" +
                    "   ([(val-id ...) val-rhs] ...)\n" +
                    "   body ...+)", s);
            List<List<Syntax>> transIdss1 = r.get("trans-id");
            List<Syntax> transRhss1 = r.get("trans-rhs");
            transIdss = map(transIdss1, ids -> map(ids, id -> Scope.add(id, sc)));
            transRhss = map(transRhss1, rhs -> Scope.add(rhs, sc));
        } else {
            r = match("(let-values ([(val-id ...) val-rhs] ...) body ...+)", s);
            transRhss = new ArrayList<>();
            transIdss = new ArrayList<>();
        }

        List<List<Syntax>> valIdss1 = r.get("val-id");
        List<List<Syntax>> valIdss = map(valIdss1, ids -> map(ids, id -> Scope.add(id, sc)));

        DupCheck.checkNoDuplicateIds(list(transIdss, valIdss), s);

        List<Syntax> valRhss = r.get("val-rhs");
        List<Syntax> bodys = r.get("body");

        // Bind each left-hand identifier and generate a corresponding key
        // fo the expand-time environment:
        List<List<Symbol>> transKeyss = map(transIdss, ids -> map(ids, Bindings::addLocalBinding));
        List<List<Symbol>> valKeyss = map(valIdss, ids -> map(ids, Bindings::addLocalBinding));

        // Evaluate compile-time expressions (if any):
        List<PList> transValss = map(transRhss, transIdss, (rhs, ids) -> evalForSyntaxesBinding(rhs, ids, ctx));

        // Fill expansion-time environment:
        Env recEnv = ctx.env.derive();
        for (List<Symbol> keys : valKeyss) {
            for (Symbol key : keys) {
                recEnv.put(key.name, Bindings.variable);
            }
        }
        each(transKeyss, transValss, (keys, vals) -> {
            each(keys, vals, (key, val) -> {
                recEnv.put(key.name, val);
            });
        });

        // Expand right-hand sides and bodyL
        ExpandContext recCtx = ctx.newEnv(recEnv);

        Syntax letrecValuesId;
        if (syntaxes) {
            // let[rec]-syntaxes+values` 翻译成 letrec-values
            letrecValuesId = Syntax.fromDatum(Core.coreStx, sym("letrec-values"));
        } else {
            letrecValuesId = r.get("let-values");
        }

        return rebuild(s, list(
                letrecValuesId,
                listColl(map(valIdss, valRhss, (ids, rhs) -> list(
                        listColl(map(ids, id -> id)),
                        rec ?   // 注意这里, rec 加 sc, 非 rec 不加, rec 新 ctx
                                expand(Scope.add(rhs, sc), recCtx) :
                                expand(rhs, ctx)
                ))),
                expandBody(bodys, sc, s, recCtx)
        ));
    }; }

    /////////////////////////////////////////////////////////////////////////////////////////////

    static Syntax datum(Syntax s, ExpandContext ctx) {
        Finder r = match("(#%datum . datum)", s);
        Syntax datum = r.get("datum");
        expect(!Procedures.isKeyword(s), "keyword misused as an expression: " + datum);

        return rebuild(s, list(
                Syntax.fromDatum(Core.coreStx, sym("quote")),
                datum
        ));
    }

    static Syntax app(Syntax s, ExpandContext ctx) {
        Finder r = match("(#%app rator rand ...)", s.e);
        Syntax app = r.get("#%app");
        Syntax rator = r.get("rator");
        List<Syntax> rands = r.get("rand");

        return rebuild(s, list(
                app,
                expand(rator, ctx),
                splice(map(rands, rand -> expand(rand, ctx)))
        ));
    }

    static Syntax top(Syntax s, ExpandContext ctx) {
        // 加入 module 时候需要处理
        throw new InterpError("unbound identifier: " + s);
    }

    static Syntax quote(Syntax s, ExpandContext ctx) {
        match("(quote datum)", s.e);
        return s;
    }

    static Syntax quasiquote(Syntax s, ExpandContext ctx) {
        Finder r = match("(quasiquote datum)", s.e);
        Syntax quasiquote = r.get("quasiquote");
        Syntax datum = r.get("datum");
        if (Syntax.isList(datum)) {
            if (isNull(datum.e)) {
                return s;
            } else {
                return rebuild(s, list(
                        quasiquote,
                        doQuasiquote(datum, ctx)
                ));
            }
        } else {
            return s;
        }
    }

    static Object doQuasiquote(Object datum, ExpandContext ctx) {
        Object lst = toSyntaxList(datum);
        if (lst == Boolean.FALSE) {
            return datum;
        } else if (isNull(lst)) {
            return datum;
        } else if (isPair(lst)) {
            Object rest = doQuasiquote(cdr(lst), ctx);
            Finder r = tryMatch("(id:unquote form)", car(lst));
            if (r != null) {
                Syntax unquote = r.get("id:unquote");
                Syntax form = r.get("form");
                String name = ((Symbol) unquote.e).name;
                if (name.equals("unquote") || name.equals("unquote-splicing")) {
                    return cons(list(unquote, expand(form, ctx)), rest);
                } else {
                    Object fst = doQuasiquote(car(lst), ctx);
                    return cons(fst, rest);
                }
            } else {
                Object fst = doQuasiquote(car(lst), ctx);
                return cons(fst, rest);
            }
        } else {
            return datum;
        }
    }

    // return PList | false
    // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
    // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
    // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
    static Object toSyntaxList(Object s) {
        if (s instanceof Syntax) {
            return toSyntaxList(((Syntax) s).e);
        } else if (isList(s)) {
            return s;
        } else if (isPair(s)) {
            Object cdr = toSyntaxList(cdr(s));
            if (cdr == Boolean.FALSE) {
                return cdr;
            } else {
                return cons(car(s), cdr); // loop
            }
        } else {
            return Boolean.FALSE;
        }
    }

    static Syntax quoteSyntax(Syntax s, ExpandContext ctx) {
        match("(quote-syntax datum)", s.e);
        return s;
    }

    static Syntax iff(Syntax s, ExpandContext ctx) {
        Finder r = match("(if tst thn els)", s);
        Syntax iff = r.get("if");
        Syntax tst = r.get("tst");
        Syntax thn = r.get("thn");
        Syntax els = r.get("els");

        return rebuild(s, list(
                iff,
                expand(tst, ctx),
                expand(thn, ctx),
                expand(els, ctx)
        ));
    }

    static Syntax withContinuationMark(Syntax s, ExpandContext ctx) {
        Finder r = match("(with-continuation-mark key val body)", s);
        Syntax wcm = r.get("with-continuation-mark");
        Syntax key = r.get("key");
        Syntax val = r.get("val");
        Syntax body = r.get("body");

        return rebuild(s, list(
                wcm,
                expand(key, ctx),
                expand(val, ctx),
                expand(body, ctx)
        ));
    }

    static Syntax begin(Syntax s, ExpandContext ctx) {
        Finder r = match("(begin e ...+)", s);
        Syntax begin = r.get("begin");
        List<Syntax> es = r.get("e");
        return rebuild(s, list(
                begin,
                splice(map(es, e -> expand(e, ctx)))
        ));
    }

    static Syntax provide(Syntax s, ExpandContext ctx) {
        Finder r = match("(provide id ...)", s);
        List<Syntax> ids = r.get("id");
        return rebuild(s, list(
                Syntax.fromDatum(Core.coreStx, sym("begin")),
                splice(map(ids, id -> list(
                        Syntax.fromDatum(Core.coreStx, sym("#%app")),
                        Syntax.fromDatum(Core.coreStx, sym("namespace-set-variable-value!")),
                        list(
                                Syntax.fromDatum(Core.coreStx, sym("quote")),
                                id
                        ),
                        id)
                ))
        ));
    }

    static Syntax set(Syntax s, ExpandContext ctx) {
        Finder r = tryMatch("(set! id rhs)", s);
        if (r == null) {
            return setField(s, ctx);
        } else {
            return setId(s, ctx);
        }
    }

    static Syntax setId(Syntax s, ExpandContext ctx) {
        Finder r = match("(set! id rhs)", s);
        Syntax set = r.get("set!");
        Syntax id = r.get("id");
        Syntax rhs = r.get("rhs");

        Binding b = Scope.resolve(id);
        expect(b != null, "no binding for assignment: " + s);
        Object t = b.lookup(ctx, s);
        expect(Bindings.isVariable(t), "cannot assign to syntax: " + s);

        return rebuild(s, list(set, id, expand(rhs, ctx)));
    }

    static Syntax setField(Syntax s, ExpandContext ctx) {
        Finder r = match("(set! (. cls-or-ins id:field) rhs)", new String[] { "." }, s);
        Syntax set = r.get("set!");
        Syntax dot = Syntax.fromDatum(Core.coreStx, sym("."));
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax field = r.get("id:field");
        Syntax rhs = r.get("rhs");
        // (set! (. className static-field-name) expr)
        // (set! (. instance-expr instance-field-name) expr)
        return rebuild(s, list(
                set,
                list(
                        dot,
                        expandClassOrInstance(clsOrIns, ctx),
                        field
                ),
                expand(rhs, ctx)
        ));
    }

    static Syntax new1(Syntax s, ExpandContext ctx) {
        Finder r = match("(new id:class-name args ...)", s);
        Syntax new1 = r.get("new");
        Syntax className = r.get("id:class-name");
        List<Syntax> args = r.get("args");
        return rebuild(s, list(
                new1,
                className,
                splice(map(args, arg -> expand(arg, ctx)))
        ));
    }

    // clojure 语法
    // https://clojure.org/reference/java_interop#_the_dot_special_form
    //   第一个操作数是JavaName 则带是静态成员访问, 否则是实例成员访问
    //
    //   第二个操作数是Name且没有参数
    //   除非有无参公开方法, 否则是字段访问
    //   如果第二个操作数Name以-开头, 只会解析成字段访问
    //
    //   如果第二个操作数是 Tuple, 则是方法调用, Tuple 第一个元素是方法名称的 Name, 也可以展开
    //
    //   (. instance-expr member-symbol)
    //   (. ClassName-symbol member-symbol)
    //   (. instance-expr -field-symbol)
    //   (. instance-expr (method-symbol args ...)) or (. instance-expr method-symbol args ...)
    //   (. ClassName-symbol (method-symbol args ...)) or (. ClassName-symbol method-symbol args ...)
    static Syntax dot(Syntax s, ExpandContext ctx) {
        Finder r = tryMatch("(. cls-or-ins (id:method args ...))", s);
        boolean isMethod = r != null;
        if (isMethod) {
           return dotMethod(s, ctx);
        } else {
            return dot1(s, ctx);
        }
    }

    // 实例静态方法调用
    // (. instance-expr (method-symbol args ...))
    // (. ClassName-symbol (method-symbol args ...))
    static Syntax dotMethod(Syntax s, ExpandContext ctx) {
        Finder r = match("(. cls-or-ins (id:method args ...))", s);
        Syntax dot = r.get(".");
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax method = r.get("id:method");
        List<Syntax> args = r.get("args");// PList

        return rebuild(s, list(
                dot,
                expandClassOrInstance(clsOrIns, ctx),
                list(
                        method,
                        splice(map(args, arg -> expand(arg, ctx)))
                )
        ));
    }

    // 实例静态属性访问
    // (. instance-expr member-symbol)
    // (. ClassName-symbol member-symbol)
    // (. instance-expr -field-symbol)
    // 实例静态方法调用
    // (. instance-expr method-symbol args ...)
    // (. ClassName-symbol method-symbol args ...)
    static Syntax dot1(Syntax s, ExpandContext ctx) {
        Finder r = match("(. cls-or-ins id:member args ...)", s);
        Syntax dot = r.get(".");
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax member = r.get("id:member");
        List<Syntax> args = r.get("args");

        return rebuild(s, list(
                dot,
                expandClassOrInstance(clsOrIns, ctx),
                member,
                splice(map(args, arg -> expand(arg, ctx)))
        ));
    }

    static Syntax expandClassOrInstance(Syntax clsOrIns, ExpandContext ctx) {
        return isJavaClassName(clsOrIns.unbox()) ? clsOrIns : expand(clsOrIns, ctx);
    }

}
