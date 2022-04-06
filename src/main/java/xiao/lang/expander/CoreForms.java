package xiao.lang.expander;

import xiao.lang.Interp;
import xiao.lang.InterpError;
import xiao.lang.RT;
import xiao.lang.Values;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.RT.*;
import static xiao.lang.Reader.isJavaClassName;
import static xiao.lang.Reader.isStaticMethodCall;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.CUtils.each;
import static xiao.lang.expander.CUtils.map;
import static xiao.lang.expander.Expansion.*;
import static xiao.lang.expander.SyntaxMatcher.match;
import static xiao.lang.expander.SyntaxMatcher.tryMatch;

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
        // 用于区分 local 的 binding. 另外，quote-syntax（不带#:local）完全展开时，会删去结果中的local scope.
        Scope sc = Scope.of(Scope.Type.local_lambda, s);

        // Parse and check formal arguments:
        PList ids = parseAndFlattenFormals(formals, sc);
        DupCheck.checkNoDuplicateIds(ids, s);
        // Bind each argument and generate a corresponding key for the
        // expand-time environment:
        ExpandContext bodyCtx = ctx.deriveEnv(new ExpandContext.Type.Body(ctx.type), ctx.scopes.add(sc));
        for (Object id : ids) {
            Symbol key = Binding.addLocalBinding((Syntax) id);
            bodyCtx.env.put(key.name, Binding.variable);
        }

        // Expand the function body:
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
            return RT.map(id -> {
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
        // 用于区分local 的 binding. 另外，quote-syntax（不带#:local）完全展开时，会删去结果中的local scope.
        Scope sc = Scope.of(rec ? Scope.Type.letrec_body : Scope.Type.local_let, s);

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
        List<List<Symbol>> transKeyss = map(transIdss, ids -> map(ids, Binding::addLocalBinding));
        List<List<Symbol>> valKeyss = map(valIdss, ids -> map(ids, Binding::addLocalBinding));

        // Evaluate compile-time expressions (if any):
        List<List<ClosureTransformer>> transValss =
                map(transRhss, transIdss, (rhs, ids) -> evalForSyntaxesBinding(rhs, ids, ctx));

        // Fill expansion-time environment:
        ExpandContext recCtx = ctx.deriveEnv(new ExpandContext.Type.Body(ctx.type), ctx.scopes.add(sc));
        for (List<Symbol> keys : valKeyss) {
            for (Symbol key : keys) {
                recCtx.env.put(key.name, Binding.variable);
            }
        }
        each(transKeyss, transValss, (keys, vals) -> {
            each(keys, vals, (key, val) -> {
                recCtx.env.put(key.name, val);
            });
        });

        // Expand right-hand sides and bodyL
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
                                Expansion.expand(Scope.add(rhs, sc), recCtx) :
                                Expansion.expand(rhs, ctx)
                ))),
                expandBody(bodys, sc, s, recCtx)
        ));
    }; }

    /////////////////////////////////////////////////////////////////////////////////////////////

    static Syntax datum(Syntax s, ExpandContext ctx) {
        Finder r = match("(#%datum . datum)", s);
        Syntax datum = r.get("datum");
        expect(!isKeyword(s), "keyword misused as an expression: " + datum);

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
                Expansion.expand(rator, ctx),
                splice(map(rands, rand -> Expansion.expand(rand, ctx)))
        ));
    }

    static Syntax top(Syntax s, ExpandContext ctx) {
        // 加入 module 时候需要处理
        throw new InterpError("unbound identifier: " + s);
    }

    static Syntax quote(Syntax s, ExpandContext ctx) {
        Finder r = match("(quote datum)", s.e);
        Syntax quote = r.get("quote");
        Syntax datum = r.get("datum");
        return rebuild(s, list(
                quote,
                datum
        ));
    }

    // 1. The syntax is of quasiquote is (quasiquote expr) which can be abbreviated as `expr.
    // 2. The syntax is of unquote is (unquote expr) which can be abbreviated as ,expr.
    // 3. (unquote expr) can only appear inside of a quasiquoted expression.
    // 4. If the quasiquoted expression contains no unquoted sub-expressions, then (quasiquote expr) is equivalent to (quote expr).
    // 5. If an (unquote expr) appears in a quasiquoted expression, then (unquote expr) is replaced by the evaluated expression expr.
    // 6. If a (quasiquote expr) appears in a quasiquoted expression, it is unchanged.
    static Syntax quasiquote(Syntax s, ExpandContext ctx) {
        Finder r = match("(quasiquote datum)", s.e);
        Syntax datum = r.get("datum");
        return rebuild(s, Quasiquote.expand(datum, ctx));
    }

    // 算法来自
    // https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1986/msg00000.html
    // https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1986/msg00002.html
    // 并添加 vector 支持
    class Quasiquote {
        public static Object expand(Syntax s, ExpandContext ctx) {
            return expand(s, ctx, s, 0);
        }

        static Object expand(Syntax s, ExpandContext ctx, Object x, int n) {
            if (Stx.isPair(x)) {
                Object car = Stx.car(x);
                Object cdr = Stx.cdr(x);

                if (equal(car, quasiquote)) {
                    check(x);
                    return quasiCons(list(quote, quasiquote), expand(s, ctx, cdr, n + 1));
                }
                if (equal(car, unquote)) {
                    check(x);
                    if (n == 0 && !Binding.isLocalBinding(s, "unquote")) {
                        Object uq = Stx.car(cdr);
                        // return uq;
                        return Expansion.expand(((Syntax) uq), ctx);
                    } else {
                        return quasiCons(list(quote, unquote), expand(s, ctx, cdr, n - 1));
                    }
                }
                // `,@a 不合法
                if (equal(car, unquoteSplice) && !Binding.isLocalBinding(s, "unquote-splicing")) {
                    if (n == 0) {
                        throw new InterpError("invalid context for unquote-splice");
                    } else {
                        return quasiCons(list(quote, unquoteSplice), expand(s, ctx, cdr, n - 1));
                    }
                }
                // 必须是 `(,@a ...)
                if (n == 0
                        && Stx.isPair(car) && equal(Stx.car(car), unquoteSplice)
                        && !Binding.isLocalBinding(s, "unquote-splicing")) {
                    check(car);
                    Object splice = Expansion.expand(((Syntax) Stx.cadr(car)), ctx);
                    Object d = expand(s, ctx, cdr, n);
                    if (Stx.isNull(d)) {
                        return splice;
                    } else {
                        return list(app, append, splice, d);
                    }
                }

                Object a = expand(s, ctx, car, n);
                Object d = expand(s, ctx, cdr, n);
                return quasiCons(a, d);
            } else if (Stx.isVector(x)) {
                // 把 vector 的 quasiquote 转换成统一的 pair 处理
                PList lst = list(((Object[]) ((Syntax) x).e));
                Syntax s_lst = Syntax.fromDatum(s, lst);
                Object exp = expand(s, ctx, s_lst, n);
                PList toArray = list(dot, exp, list(sym("toArray")));
                // return dot(Syntax.fromDatum(s, toArray), ctx);
                return Syntax.fromDatum(s, toArray);
            } else {
                return list(quote, x);
            }
        }

        static Object quasiCons(Object a, Object d) {
            if (Stx.isPair(d)) {
                Object card = Stx.car(d);
                Object cdrd = Stx.cdr(d);
                if (equal(card, quote)) {
                    Object qd = Stx.car(cdrd);
                    if (Stx.isPair(a) && equal(Stx.car(a), quote)) {
                        Object qa = Stx.cadr(a);
                        return list(quote, cons(qa, qd));
                    } else {
                        if (Stx.isNull(qd)) {
                            return list(app, list, a);
                        } else {
                            return list(app, listStar, a, d);
                        }
                    }
                } else if (equal(card, list) || equal(card, listStar)) {
                    expect(Stx.isList(cdrd), "");
                    if (card instanceof Syntax) {
                        card = ((Syntax) card).e;
                    }
                    return list(card, a, splice(((PList) Stx.toList(cdrd))));
                } else {
                    return list(app, listStar, a, d);
                }
            } else {
                return list(app, listStar, a, d);
            }
        }

        static void check(Object x) {
            expect(Stx.isPair(Stx.cdr(x)) && Stx.isNull(Stx.cdr(Stx.cdr(x))),
                    "invalid form: " + Stx.car(x));
        }

        static boolean equal(Object x, Syntax id) {
            if (x instanceof Syntax) {
                return equal(((Syntax) x).e, id);
            } else {
                return id.e.equals(x);
            }
        }

        final static Syntax quasiquote = Syntax.fromDatum(Core.coreStx, sym("quasiquote"));
        final static Syntax quote = Syntax.fromDatum(Core.coreStx, sym("quote"));
        final static Syntax unquote = Syntax.fromDatum(Core.coreStx, sym("unquote"));
        final static Syntax unquoteSplice = Syntax.fromDatum(Core.coreStx, sym("unquote-splicing"));

        final static Syntax app = Syntax.fromDatum(Core.coreStx, sym("#%app"));
        final static Syntax append = Syntax.fromDatum(Core.coreStx, sym("append"));
        final static Syntax listStar = Syntax.fromDatum(Core.coreStx, sym("list*"));
        final static Syntax list = Syntax.fromDatum(Core.coreStx, sym("list"));

        final static Syntax dot = Syntax.fromDatum(Core.coreStx, sym("."));
    }

    // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
    // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
    // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
    static Optional<PList> toSyntaxList(Object s) {
        if (s instanceof Syntax) {
            return toSyntaxList(((Syntax) s).e);
        } else if (isList(s)) {
            return Optional.of(((PList) s));
        } else if (isPair(s)) {
            return toSyntaxList(cdr(s)).map(cdr -> cons(car(s), cdr));
        } else {
            return Optional.empty();
        }
    }

    static Syntax doQuoteSyntax(Syntax d, ExpandContext ctx) {
        ScopeSet scs = d.scopes.remove(ctx.scopes);
        if (ctx.useSiteScopes != null) {
            scs = scs.remove(ctx.useSiteScopes);
        }
        return Syntax.of(d.e, scs.set.toArray(new Scope[0]));
    }

    // creates a quoted syntax, with its lexical source from the place it appears.
    // Similar to quote, but produces a syntax object that preserves the lexical information
    // attached to datum at expansion time.
    static Syntax quoteSyntax(Syntax s, ExpandContext ctx) {
        Finder r = tryMatch("(quote-syntax datum)", s.e);
        if (r == null) {
            match("(quote-syntax datum local)", s.e);
            return s;
        } else {
            // 参考 racket 文档
            Syntax d = r.get("datum");
            Syntax datumS = doQuoteSyntax(d, ctx);
            return rebuild(s, list(r.get("quote-syntax"), datumS));
        }
    }

    // 不支持模板, 相当于 quote-syntax
    static Syntax syntax(Syntax s, ExpandContext ctx) {
        Finder r = match("(syntax id)", s.e);
        Syntax d = r.get("id");
        Syntax datumS = doQuoteSyntax(d, ctx);
        return rebuild(s, list(sym("quote-syntax"), datumS));
    }

    static Syntax iff(Syntax s, ExpandContext ctx) {
        Finder r = match("(if tst thn els)", s);
        Syntax iff = r.get("if");
        Syntax tst = r.get("tst");
        Syntax thn = r.get("thn");
        Syntax els = r.get("els");

        return rebuild(s, list(
                iff,
                Expansion.expand(tst, ctx),
                Expansion.expand(thn, ctx),
                Expansion.expand(els, ctx)
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
                Expansion.expand(key, ctx),
                Expansion.expand(val, ctx),
                Expansion.expand(body, ctx)
        ));
    }

    static Syntax begin(Syntax s, ExpandContext ctx) {
        Finder r = match("(begin e ...+)", s);
        Syntax begin = r.get("begin");
        List<Syntax> es = r.get("e");
        return rebuild(s, list(
                begin,
                splice(map(es, e -> Expansion.expand(e, ctx)))
        ));
    }

    static Syntax provide(Syntax s, ExpandContext ctx) {
        Finder r = match("(provide id ...)", s);
        List<Syntax> ids = r.get("id");

        List<Syntax> nonTransformerIds = new ArrayList<>(ids.size());
        for (Syntax id : ids) {
            Binding binding = Scope.resolve(id);
            expect(binding != null, "illegal use of syntax: " + s);
            Object t = binding.lookup(ctx, id);
            if (Binding.isVariable(t)) {
                nonTransformerIds.add(id);
            } else if (Binding.isTransformer(t)) {
                throw new InterpError("unsupported provide transformer: " + id);
            } else {
                throw new InterpError("illegal use of syntax: " + s);
            }
        }

        // eval 期间导出 procedure
        return rebuild(s, list(
                Syntax.fromDatum(Core.coreStx, sym("begin")),
                splice(map(nonTransformerIds, id -> list(
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
        expect(Binding.isVariable(t), "cannot assign to syntax: " + s);

        return rebuild(s, list(set, id, Expansion.expand(rhs, ctx)));
    }

    static Syntax setField(Syntax s, ExpandContext ctx) {
        Finder r = match("(set! (. cls-or-ins id:field) rhs)", s, ".");
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
                Expansion.expand(rhs, ctx)
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
                splice(map(args, arg -> Expansion.expand(arg, ctx)))
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
                        splice(map(args, arg -> Expansion.expand(arg, ctx)))
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
                splice(map(args, arg -> Expansion.expand(arg, ctx)))
        ));
    }

    static Syntax expandClassOrInstance(Syntax clsOrIns, ExpandContext ctx) {
        if (isJavaClassName(clsOrIns.unbox())) {
            assert !isStaticMethodCall(clsOrIns.unbox());
            return clsOrIns;
        } else {
            return Expansion.expand(clsOrIns, ctx);
        }
    }

}
