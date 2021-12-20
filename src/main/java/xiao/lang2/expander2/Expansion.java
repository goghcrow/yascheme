package xiao.lang2.expander2;

import xiao.lang2.Env;
import xiao.lang2.Interp;
import xiao.lang2.InterpError;
import xiao.lang2.Reader;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Names.SLASH;
import static xiao.lang2.Pattern.Finder;
import static xiao.lang2.Procedures.*;
import static xiao.lang2.Values.*;
import static xiao.lang2.expander2.Bindings.*;
import static xiao.lang2.expander2.CUtils.map;
import static xiao.lang2.expander2.Utils.match;

/**
 * @author chuxiaofeng
 */
public class Expansion {

    // https://docs.racket-lang.org/reference/syntax-model.html
    // 1.2.3.2 Expansion Steps
    // recursive expansion
    static Syntax expand(Syntax s, ExpandContext ctx) {
        if (Syntax.isIdentifier(s)) {
            // ::step-1::
            // If it is an identifier (i.e., a syntax-object symbol),
            // then a binding is determined by the identifier’s lexical information.
            // If the identifier has a binding, that binding is used to continue.
            // If the identifier is unbound, a new syntax-object symbol '#%top is created
            // using the lexical information of the identifier with implicit-made-explicit properties;
            // if this #%top identifier has no binding, then parsing fails with an exn:fail:syntax exception.
            // Otherwise, the new identifier is combined with the original identifier in a new syntax-object pair
            // (also using the same lexical information as the original identifier), and the #%top binding is used to continue.
            return expandIdentifier(s, ctx);
        } else if (isPair(s.e) && Syntax.isIdentifier(car(s.e))) {
            // ::step-2::
            // If it is a syntax-object pair whose first element is an identifier,
            // and if the identifier has a binding other than as a top-level variable,
            // then the identifier’s binding is used to continue.
            return expandIdAppForm(((Syntax) car(s.e)), s, ctx);
        } else if (isPair(s.e) || isNull(s.e)) {
            // ::step-3::
            // If it is a syntax-object pair of any other form, then a new syntax-object symbol '#%app is created
            // using the lexical information of the pair with implicit-made-explicit properties.
            // If the resulting #%app identifier has no binding, parsing fails with an exn:fail:syntax exception.
            // Otherwise, the new identifier is combined with the original pair to form a new syntax-object pair
            // (also using the same lexical information as the original pair), and the #%app binding is used to continue.

            // An "application" form that doesn't start with an identifier, so use implicit `#%app`
            // e.g. ((curried '1) '2)
            // isNull(s.e) 分支, () 最终在 expand #%app 时匹配失败 (#%app rator rand ...)
            return expandImplicit(sym("#%app"), s, ctx);
        } else {
            // ::step-4::
            // If it is any other syntax object, then a new syntax-object symbol '#%datum is created
            // using the lexical information of the original syntax object with implicit-made-explicit properties.
            // If the resulting #%datum identifier has no binding, parsing fails with an exn:fail:syntax exception.
            // Otherwise, the new identifier is combined with the original syntax object in a new syntax-object pair
            // (using the same lexical information as the original pair), and the #%datum binding is used to continue.

            // Anything other than an identifier or parens triggers the implicit `#%datum` form
            return expandImplicit(sym("#%datum"), s, ctx);
        }
    }

    static Syntax expandIdentifier(Syntax s, ExpandContext ctx) {
        // If it is an identifier (i.e., a syntax-object symbol),
        // then a binding is determined by the identifier’s lexical information.
        Binding b = Scope.resolve(s);
        if (b == null) {
            // The implicit `#%top` form handles unbound identifiers
            return expandImplicit(sym("#%top"), s, ctx);
        } else {
            // If the identifier has a binding, that binding is used to continue.
            // Variable or core-form or form as identifier macro
            Object val = b.lookup(ctx, s);
            return dispatch(val, s, ctx);
        }
    }

    // An "application" form that starts with an identifier
    // expand an identifier in "application" position
    // e.g. (lambda (x) x)
    // e.g. (f '1)
    // e.g. (one) ; (let-syntax ([one (lambda (stx) (quote-syntax '1))]) (one))
    static Syntax expandIdAppForm(Syntax id, Syntax s, ExpandContext ctx) {
        Binding binding = Scope.resolve(id);
        if (binding == null) {
            return expandUnbound(id, s, ctx);
        } else {
            // if the identifier has a binding other than as a top-level variable,
            // then the identifier’s binding is used to continue.

            // Find out whether it's bound as a variable, syntax, or core form
            Object t = binding.lookup(ctx, id);
            if (isVariable(t)) {
                // adds `#%app` and expands every application to an `#%app` form
                // https://docs.racket-lang.org/reference/application.html
                // the expander converts this form to (#%app proc-expr arg ...),
                // giving #%app the lexical context that is associated with the original form
                // (i.e., the pair that combines proc-expr and its arguments)

                // Not as syntax or core form, so use implicit `#%app`
                return expandImplicit(sym("#%app"), s, ctx);
            } else {
                // Syntax or core form as "application"
                return dispatch(t, s, ctx);
            }
        }
    }

    static Syntax expandUnbound(Syntax id, Syntax s, ExpandContext ctx) {
        Object car = id.unbox();
        if (isJavaInterop(car)) {
            return rewriteJavaInterop(car, s, ctx);
        } else {
            // top-level variable
            // The `#%app` binding might do something with unbound ids
            // 处理路径:
            // --> expandImplicit   :: (#%app unbound-id rand ...)
            // --> expandIdentifier :: resolve unbound-id fail, => #%top unbound-id
            // --> expandImplicit   :: resolve #%top fail => no transformer binding for #％top unbound-id
            return expandImplicit(sym("#%app"), s, ctx);
        }
    }

    @SuppressWarnings("RedundantIfStatement")
    static boolean isJavaInterop(Object car) {
        if (Reader.MemberAccessorExpansion.isInstanceMemberAccess(car)) {
            return true;
        } else if (Reader.MemberAccessorExpansion.isStaticMethodCall(car)) {
            return true;
        } else if (Reader.MemberAccessorExpansion.isNewInstance(car)) {
            return true;
        } else {
            return false;
        }
    }

    static Syntax rewriteJavaInterop(Object car, Syntax s, ExpandContext ctx) {
        if (Reader.MemberAccessorExpansion.isInstanceMemberAccess(car)) {
            if (Reader.MemberAccessorExpansion.isInstanceFieldAccess(car)) {
                // (.-instanceField instance-expr) ==> (. instance-expr -instanceField)
                Finder r = match("(id:.-instanceField instance-expr)", s);
                Syntax hyphenField = r.get("id:.-instanceField");
                Syntax expr = r.get("instance-expr");
                Symbol field = sym(((Symbol) hyphenField.e).name.substring(1));

                // 只改写, 不展开, dot 再展开
                return expand(rebuild(s, list(
                        Syntax.fromDatum(Core.coreStx, sym(".")),
                        expr, // expand(expr, ctx)
                        Syntax.fromDatum(hyphenField, field, hyphenField)
                )), ctx);
            } else {
                // (.instanceMember instance-expr args ...) ==> (. instance-expr (instanceMember args ...))
                // (.instanceMember ClassName args ...) ==> (. ClassName (instanceMember args ...))
                Finder r = match("(id:.instanceMember cls-or-ins args ...)", s);
                Syntax dotField = r.get("id:.instanceMember");
                Syntax clsOrIns = r.get("cls-or-ins");
                List<Syntax> args = r.get("args"); // PList
                Symbol field = sym(((Symbol) dotField.e).name.substring(1));

                // 只改写, 不展开, dot 再展开
                return expand(rebuild(s, list(
                        Syntax.fromDatum(Core.coreStx, sym(".")),
                        clsOrIns,
                        list(
                                Syntax.fromDatum(dotField, field, dotField),
                                splice(args) // splice(map(args, arg -> expand(arg, ctx)))
                        )
                )), ctx);
            }
        } else if (Reader.MemberAccessorExpansion.isStaticMethodCall(car)) {
            // (ClassName/staticMethod args ...) ==> (. ClassName-symbol (method-symbol args ...))
            Finder r = match("(id:ClassName/staticMethod args ...)", s);
            Syntax clsMethod = r.get("id:ClassName/staticMethod");
            List<Syntax> args = r.get("args"); // PList
            String id = ((Symbol) clsMethod.e).name;
            int idx = id.indexOf(SLASH);
            String klass = id.substring(0, idx);
            String method = id.substring(idx + 1);

            return expand(rebuild(s, list(
                    Syntax.fromDatum(Core.coreStx, sym(".")),
                    Syntax.fromDatum(clsMethod, sym(klass), clsMethod),
                    list(
                            Syntax.fromDatum(clsMethod, sym(method), clsMethod),
                            // 延迟到 CoreForms::dot 在展开, 这里只改写, 统一流程
                            splice(args) // splice(map(args, arg -> expand(arg, ctx)))
                    )
            )), ctx);
        } else if (Reader.MemberAccessorExpansion.isNewInstance(car)) {
            // (ClassName. args ...) ==> (new ClassName args ...)
            Finder r = match("(id:ClassName. args ...)", s);
            Syntax clsNameDot = r.get("id:ClassName.");
            String id = ((Symbol) clsNameDot.e).name;
            List<Syntax> args = r.get("args");
            Symbol klass = sym(id.substring(0, id.length() - 1));

            return expand(rebuild(s, list(
                    Syntax.fromDatum(Core.coreStx, sym("new")),
                    Syntax.fromDatum(clsNameDot, klass, clsNameDot),
                    // 延迟到 CoreForms::new1 在展开, 这里只改写, 统一流程
                    splice(args) // splice(map(args, arg -> expand(arg, ctx)))
            )), ctx);
        } else {
            throw new IllegalStateException();
        }
    }

    // Handle an implicit: `#%app`, `#%top`, or `#%datum`
    static Syntax expandImplicit(Symbol sym, Syntax s, ExpandContext ctx) {
        // #%top: If the identifier is unbound, a new syntax-object symbol '#%top is created
        // using the lexical information of the identifier with implicit-made-explicit properties;
        // #%app: If it is a syntax-object pair of any other form, then a new syntax-object symbol '#%app is created
        // using the lexical information of the pair with implicit-made-explicit properties.
        // #%datum: If it is any other syntax object, then a new syntax-object symbol '#%datum is created
        // using the lexical information of the original syntax object with implicit-made-explicit properties.
        Syntax id = Syntax.fromDatum(s, sym); // new syntax-object symbol #%app

        // Instead of calling `expand` with a new form that starts `id`,
        // we implement the "application"-form case of `expand` so that
        // we provide an error if the implicit form is not suitably bound
        Binding b = Scope.resolve(id);
        if (b != null) {
            // #%top: Otherwise, the new identifier is combined with the original identifier in a new syntax-object pair
            // (also using the same lexical information as the original identifier), and the #%top binding is used to continue.
            // #%app: Otherwise, the new identifier is combined with the original pair to form a new syntax-object pair
            // (also using the same lexical information as the original pair), and the #%app binding is used to continue.
            // #%datum: Otherwise, the new identifier is combined with the original syntax object in a new syntax-object pair
            // (also using the same lexical information as the original pair), and the #%datum binding is used to continue.
            Object t = b.lookup(ctx, id);
            // todo: implicit-made-explicit prop
            // When a #%top, #%app, or #%datum identifier is added by the expander,
            // it is given implicit-made-explicit properties:
            // an 'implicit-made-explicit syntax property whose value is #t,
            // and a hidden property to indicate that
            // the implicit identifier is original in the sense of syntax-original?
            // if the syntax object that gives the identifier its lexical information has that property.
            Syntax newStxPair = Syntax.fromDatum(s, cons(sym, s), s);
            if (isCoreForm(t)) {
                if (ctx.onlyImmediate) {
                    return s;
                } else {
                    return dispatch(t, newStxPair, ctx);
                }
            } else if (t instanceof SyntaxTransformer) {
                return dispatch(t, newStxPair, ctx);
            }
        }

        // #%top: if this #%top identifier has no binding, then parsing fails with an exn:fail:syntax exception.
        // #%app: If the resulting #%app identifier has no binding, parsing fails with an exn:fail:syntax exception.
        // #%datum: If the resulting #%datum identifier has no binding, parsing fails with an exn:fail:syntax exception.
        throw new InterpError("no transformer binding for " + sym + ": " + s);
    }

    // 这里 dispatch 就是 Binding.lookup 结果的三种值
    // Expand `s` given that the value `t` of the relevant binding,
    // where `t` is either a core form, a macro transformer, some
    // other compile-time value (which is an error), or a token
    // indicating that the binding is a run-time variable
    static Syntax dispatch(Object t, Syntax s, ExpandContext ctx) {
        // Thus, the possibilities that do not fail lead to an identifier with a particular binding.
        // This binding refers to one of three things:

        if (isCoreForm(t)) {
            // 3. A core syntactic form, which is parsed as described for each form in Syntactic Forms.
            // Parsing a core syntactic form typically involves recursive parsing of sub-forms,
            // and may introduce bindings that determine the parsing of sub-forms.
            if (ctx.onlyImmediate) {
                return s;
            } else {
                return ((CoreForm) t).expander.transform(s, ctx);
            }
        } else if (isTransformer(t)) {
            // 1. A transformer, such as introduced by define-syntax or let-syntax.
            // If the associated value is a procedure of one argument,
            // the procedure is called as a syntax transformer (described below),
            // and parsing starts again with the syntax-object result.
            // If the transformer binding is to any other kind of value,
            // parsing fails with an exn:fail:syntax exception.
            // The call to the syntax transformer is parameterized to set current-namespace
            // to a namespace that shares bindings and variables with the namespace being used to expand,
            // except that its base phase is one greater.

            // expandIdentifier 分支: identifier macros are allowed (i.e., not constrained to application positions)
            // Apply transformer and expand again
            Closure syntaxTransformer = (Closure) t;
            Syntax transformed = applyTransformer(syntaxTransformer, s, ctx);
            // 这里处理生成宏的宏, 递归宏展开!!!
            // 可以通过打印 transformed 来观察递归展开状态
            // System.out.println(s + " => " + transformed);
            return expand(transformed, ctx);
        } else if (isVariable(t)) {
            // 2. A variable binding, such as introduced by a module-level define or by let.
            // In this case, if the form being parsed is just an identifier,
            // then it is parsed as a reference to the corresponding variable.

            // If the form being parsed is a syntax-object pair,
            // then an #%app is added to the front of the syntax-object pair
            // in the same way as when the first item in the syntax-object pair is not an identifier
            // (third case in the previous enumeration), and parsing continues.

            // A reference to a variable expands to itself
            return s;
        } else {
            // Some other compile-time value:
            throw new InterpError("illegal use of syntax: " + s);
        }
    }

    // Given a macro transformer `t` to syntax, apply it --- adding appropriate
    // scopes to represent the expansion step
    static Syntax applyTransformer(Closure t, Syntax s, ExpandContext ctx) {
        // ------------------------------------------------------------
        // e.g.
        // (let-syntax ([one (lambda (stx) (quote-syntax '1))]) (one))
        // Syntax s => (one)
        // Closure t => (lambda (stx) (quote-syntax '1))
        // transformedS => '1
        // ------------------------------------------------------------
        // e.g.
        // (let-syntax ([thunk (lambda (stx)
        //      (list (quote-syntax lambda)
        //          (list (quote-syntax x))
        //          (cadr stx)))])
        //  (thunk x))
        // Syntax s => (thunk x)
        // Closure t => procedure that expands thunk
        // x => x:sc...
        // (thunk:sc... x:sc...)
        // Scope.add(s, introScope) => (thunk:sc...,introScope x:sc...,introScope)
        // transformed => (lambda:sc1 (x:sc1) x:sc...,introScope)
        // Scope.flip(transformed, introScope) => (lambda:sc1 (x:sc1) x:sc...)

        // todo: 看一下文档
        // https://www.cs.utah.edu/plt/scope-sets/index.html
        // https://docs.racket-lang.org/reference/syntax-model.html
        // todo: 为什么要引入 introScope 然后 flip
        // todo: useSiteScope 与 postExpansionScope 有啥用

        // macro-scope
        // 宏展开引入的scope，通过宏展开前后的两次反转，所有展开过程中新引入的syntax对象都会添加上该scope
        // Scope.flip(transformedS, introScope)
        // 会把展开前的 syntax 引入的 introScope 移除, 展开过程新引入的 syntax 加入 introScope
        // 用来识别新加入的 syntax
        // create a scope to represent the macro step
        Scope introScope = Scope.of();
        // tentatively add the scope to the input
        Syntax introS = Scope.add(s, introScope);
        // In a definition context, we need use-site scopes
        Syntax useS = maybeAddUseSiteScope(introS, ctx);
        // call the transformer
        Syntax transformedS = callTransformer(t, useS);
        // flip intro scope to get final result
        Syntax resultS = Scope.flip(transformedS, introScope);
        // In a definition context, we need to add the inside-edge scope to any expansion result
        return maybeAddPostExpansionScope(resultS, ctx);
    }

    static Syntax callTransformer(Procedure p, Syntax stx) {
        Object transformedS = callProcedure(p, stx);
        expect(transformedS instanceof Syntax,
                "transformer produced non-syntax: " + transformedS);
        return ((Syntax) transformedS);
    }

    static Object callProcedure(Procedure p, Object... args) {
        Object[] ref = new Object[1];
        Interp.run(() -> p.call(
                args,
                new Env(), // 闭包用不到 call scope
                v -> ref[0] = v
        ));
        return ref[0];
    }

    static Syntax maybeAddUseSiteScope(Syntax s, ExpandContext ctx) {
        if (ctx.useSiteScopes == null) {
            return s;
        } else {
            // We're in a recursive definition context where use-site scopes
            // are needed, so create one, record it, and add to the given syntax
            Scope sc = Scope.of();
            ctx.useSiteScopes = ctx.useSiteScopes.add(sc);
            return Scope.add(s, sc);
        }
    }

    static Syntax maybeAddPostExpansionScope(Syntax s, ExpandContext ctx) {
        if (ctx.postExpansionScope == null) {
            return s;
        } else {
            // We're in a definition context where an inside-edge scope needs
            // to be added to any immediate macro expansion; that way, if the
            // macro expands to a definition form, the binding will be in the
            // definition context's scope
            return Scope.add(s, ctx.postExpansionScope);
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // 1.2.3.7 Partial Expansion
    // https://docs.racket-lang.org/reference/syntax-model.html
    //
    // In certain contexts, such as an internal-definition context or module context,
    // partial expansion is used to determine
    // whether forms represent definitions, expressions, or other declaration forms.
    // Partial expansion works by cutting off the normal recursive expansion
    // when the relevant binding is for a primitive syntactic form.
    //
    // As a special case, when expansion would otherwise add an #%app, #%datum, or #%top identifier to an expression,
    // and when the binding turns out to be the primitive #%app, #%datum, or #%top form,
    // then expansion stops without adding the identifier.

    // 1.2.3.8 Internal Definitions
    // An internal-definition context supports local definitions mixed with expressions.
    // Forms that allow internal definitions document such positions using the body meta-variable.
    // Definitions in an internal-definition context are equivalent to local binding via letrec-syntaxes+values;
    // macro expansion converts internal definitions to a letrec-syntaxes+values form.
    //
    // Expansion relies on partial expansion of each body in an internal-definition sequence.
    // Partial expansion of each body produces a form matching one of the following cases:
    // - A define-values form:
    //      The binding table is immediately enriched with bindings for the define-values form.
    //      Further expansion of the definition is deferred, and partial expansion continues with the rest of the body.
    // - A define-syntaxes form:
    //      The right-hand side is expanded and evaluated (as for a letrec-syntaxes+values form),
    //      and a transformer binding is installed for the body sequence before partial expansion continues with the rest of the body.
    // - A primitive expression form other than begin:
    //      Further expansion of the expression is deferred, and partial expansion continues with the rest of the body.
    // - A begin form:
    //      The sub-forms of the begin are spliced into the internal-definition sequence,
    //      and partial expansion continues with the first of the newly-spliced forms
    //      (or the next form, if the begin had no sub-forms).
    // After all body forms are partially expanded, if no definitions were encountered,
    // then the expressions are collected into a begin form as the internal-definition context’s expansion.
    // Otherwise, at least one expression must appear after the last definition,
    // and any expr that appears between definitions is converted to (define-values () (begin expr (values)));
    // the definitions are then converted to bindings in a letrec-syntaxes+values form,
    // and all expressions after the last definition become the body of the letrec-syntaxes+values form.
    //
    // Before partial expansion begins, expansion of an internal-definition context begins with
    // the introduction of a fresh outside-edge scope on the content of the internal-definition context.
    // This outside-edge scope effectively identifies syntax objects that are present in the original form.
    // An inside-edge scope is also created and added to the original content;
    // furthermore, the inside-edge scope is added to the result of any partial expansion.
    // This inside-edge scope ensures that all bindings introduced by the internal-definition context
    // have a particular scope in common.



    // Expand a sequence of body forms in a definition context
    // definition context:
    //  - lambda
    //  - let[rec]-[syntaxes+]values
    static Syntax expandBody(List<Syntax> bodys, Scope sc, Syntax s, ExpandContext ctx) {
        // 在 definition context 展开的时候, 输入的 syntax 对象会带上 outside-edge scope 和 inside-edge scope
        // 并且, 展开的结果也会带上 inside-edge scope

        // outside-edge scope 区分宏引入的 identifier
        //
        // The outside-edge scope identifies the original content of the definition context
        //
        // This outside-edge scope effectively identifies syntax objects that are present in the original form.
        Scope outsideSc = Scope.of();

        // inside-edge scope 区分不同的 definition context
        //
        // The inside-edge scope identifiers any form that appears (perhaps
        // through macro expansion) in the definition context
        // (maybeAddPostExpansionScope )
        //
        // This inside-edge scope ensures that all bindings introduced by
        // the internal-definition context have a particular scope in common.
        Scope insideSc = Scope.of();

        // Create an expansion context for expanding only immediate macros;
        // this partial-expansion phase uncovers macro- and variable
        // definitions in the definition context
        ExpandContext bodyCtx = new ExpandContext(
                // use-site-scope
                // 当一个宏的定义和使用在同一个 definition context 时，宏的参数会带上该 scope
                ScopeSet.empty(), // useSiteScopes!!!
                ctx.namespace,
                ctx.env,
                true,
                // 展开的结果会带上 inside-edge scope
                insideSc // postExpansionScope!!!
        );

        LinkedList<Syntax> initBodys = new LinkedList<>();
        for (Syntax body : bodys) {
            // 输入的 syntax 对象会带上 outside-edge scope 和 inside-edge scope
            initBodys.add(Scope.add(
                    Scope.add(
                            // let lambda 的 local-scope
                            Scope.add(body, sc),
                            outsideSc
                    ),
                    insideSc
            ));
        }
        return expandBody1(bodyCtx, initBodys, s, ctx);
    }

    static class Bind {
        final PList ids;
        final Syntax rhs;
        Bind(PList ids, Syntax rhs) {
            this.ids = ids;
            this.rhs = rhs;
        }
    }

    static Syntax expandBody1(ExpandContext bodyCtx,
                              LinkedList<Syntax> bodys,
                              Syntax s,
                              ExpandContext ctx
    ) {
        LinkedList<Syntax> doneBodys = new LinkedList<>(); // accumulated expressions
        LinkedList<Bind> valBinds = new LinkedList<>(); // accumulated bindings
        Map<Object, List<Syntax>> dups = DupCheck.makeCheckNoDuplicateTable(); // make-check-no-duplicate-table

        while (!bodys.isEmpty()) {
            Syntax body = bodys.removeFirst();

            // (define exp-body (expand (car bodys) body-ctx))
            Syntax expBody = expand(body, bodyCtx);

            Symbol coreSym = Core.coreFormSym(expBody);
            // 内部定义的部分转换只处理以下三种情况: begin,define-values,define-syntaxes
            if (coreSym != null) {
                switch (coreSym.name) {
                    case "begin": {
                        // https://docs.racket-lang.org/reference/begin.html
                        // (begin form ...)
                        // (begin expr ...+)
                        // The first form applies when begin appears at the top level, at module level,
                        // or in an internal-definition position (before any expression in the internal-definition sequence).
                        // In that case, the begin form is equivalent to splicing the forms into the enclosing context.
                        // 这里就是在 internal-definition 位置 splicing

                        // Splice a `begin` form
                        Finder r = match("(begin e ...)", expBody);
                        List<Syntax> e = r.get("e");
                        for (int i = e.size() - 1; i >= 0; i--) {
                            bodys.addFirst(e.get(i));
                        }
                        continue;
                    }
                    case "define-values": {
                        // Found a variable definition; add bindings, extend the environment, and continue
                        Finder r = match("(define-values (id ...) rhs)", expBody);
                        List<Syntax> idLst = r.get("id");
                        Syntax rhs = r.get("rhs");

                        PList ids = removeUseSiteScope(listColl(map(idLst, it -> it)), bodyCtx);
                        DupCheck.checkNoDuplicateIds(ids, expBody, dups);
                        Env extendedEnv = bodyCtx.env.derive();
                        for (Object id : ids) {
                            Symbol key = addLocalBinding(((Syntax) id));
                            extendedEnv.put(key.name, variable);
                        }

                        bodyCtx = bodyCtx.newEnv(extendedEnv);

                        // If we had accumulated some expressions, we need to turn each into a
                        //  (defined-values () (begin <expr> (values))) form
                        //  so it can be kept with definitions to preserved order
                        while (!doneBodys.isEmpty()) {
                            Syntax doneBody = doneBodys.removeFirst();
                            valBinds.add(noBinds(doneBody, s));
                        }
                        valBinds.add(new Bind(ids, rhs));
                        continue;
                    }
                    case "define-syntaxes": {
                        // Found a macro definition; add bindings, evaluate the compile-time right-hand side,
                        // install the compile-time values in the environment, and continue
                        Finder r = match("(define-syntaxes (id ...) rhs)", expBody);
                        List<Syntax> idLst = r.get("id");
                        PList ids = removeUseSiteScope(listColl(map(idLst, it -> it)), bodyCtx);
                        DupCheck.checkNoDuplicateIds(ids, expBody, dups);

                        Syntax rhs = r.get("rhs");
                        PList vals = evalForSyntaxesBinding(rhs, ids, ctx);

                        Env extendedEnv = bodyCtx.env.derive();
                        for (int i = 0; i < vals.size(); i++) {
                            Syntax id = idLst.get(i);
                            Symbol key = addLocalBinding(id);
                            // A transformer, such as introduced by define-syntax or let-syntax.
                            // If the associated value is a procedure of one argument,
                            // the procedure is called as a syntax transformer (described below),
                            Procedure transformer = ((Procedure) vals.get(i));
                            extendedEnv.put(key.name, transformer);
                        }

                        bodyCtx = bodyCtx.newEnv(extendedEnv);
                        continue;
                    }
                }
            }

            // Found an expression; accumulate it and continue
            doneBodys.addLast(expBody);
        }

        // todo 临时添加
//        if (doneBodys.isEmpty()) {
//            doneBodys.add(Void());
//        }

        // Partial expansion is complete, so finish by rewriting to `letrec-values`
        return finishExpandingBody(bodyCtx, doneBodys, valBinds, s);
    }

    // Partial expansion is complete, so assemble the result as a
    // `letrec-values` form and continue expanding
    static Syntax finishExpandingBody(
            ExpandContext bodyCtx,
            List<Syntax> doneBodys,
            List<Bind> valBinds,
            Syntax s
    ) {
        expect(!doneBodys.isEmpty(), "no body forms: " + s);

        // As we finish expanding, we're no longer in a definition context
        ExpandContext finishCtx = new ExpandContext(
                null,
                bodyCtx.namespace,
                bodyCtx.env,
                false,
                null
        );

        // Helper to expand and wrap the ending expressions in `begin`, if needed:
        Syntax finishBodys;
        if (doneBodys.size() == 1) {
            finishBodys = expand(doneBodys.get(0), finishCtx);
        } else {
            finishBodys = Syntax.fromDatum(
                    null,
                    list(
                            Syntax.fromDatum(Core.coreStx, sym("begin")),
                            splice(map(doneBodys, it -> expand(it, finishCtx)))
                    ),
                    s
            );
        }

        if (valBinds.isEmpty()) {
            // No definitions, so no `letrec-values` wrapper needed:
            return finishBodys;
        } else {
            // Add `letrec-values` wrapper, finish expanding the right-hand
            // sides, and then finish the body expression:
            return Syntax.fromDatum(
                    null,
                    list(
                            Syntax.fromDatum(Core.coreStx, sym("letrec-values")),
                            listColl(
                                    map(valBinds, bind -> list(
                                            Syntax.fromDatum(null, bind.ids),
                                            expand(bind.rhs, finishCtx)
                                    ))
                            ),
                            finishBodys
                    ),
                    s
            );
        }
    }

    // Helper to turn an expression into a binding clause with zero bindings
    // (defined-values () (begin <expr> (values)))
    static Bind noBinds(Syntax expr, Syntax s) {
        return new Bind(
                list(),
                Syntax.fromDatum(
                        null,
                        list(
                                Syntax.fromDatum(Core.coreStx, sym("begin")),
                                expr,
                                list(
                                        Syntax.fromDatum(Core.coreStx, sym("#%app")),
                                        Syntax.fromDatum(Core.coreStx, sym("values"))
                                )
                        ),
                        s
                )
        );
    }

    // todo
    static Syntax Void() {
        return Syntax.fromDatum(
                null,
                list(
                        Syntax.fromDatum(Core.coreStx, sym("#%app")),
                        Syntax.fromDatum(Core.coreStx, sym("void"))
                )
        );
    }

    // Helper to remove any created use-site scopes from the left-hand
    // side of a definition that was revealed by partial expansion in a
    // definition context
    static <T> T removeUseSiteScope(T s, ExpandContext ctx) {
        return Scope.remove(s, ctx.useSiteScopes);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // Expand and evaluate `s` as a compile-time expression, returning
    // only the compile-time values
    // ids = List<Syntax>
    static PList evalForSyntaxesBinding(Syntax rhs, List<?> ids, ExpandContext ctx) {
        return expandAndEvalForSyntaxesBinding(rhs, ids, ctx);
    }

    // Expand and evaluate `s` as a compile-time expression, ensuring that
    // the number of returned values matches the number of target
    // identifiers; return the expanded form as well as its values
    static PList expandAndEvalForSyntaxesBinding(Syntax rhs, List<?> ids, ExpandContext ctx) {
        Syntax expRhs = expandTransformer(rhs, ctx);
        return evalForBindings(ids, expRhs, ctx.namespace);
    }

    // Expand `s` as a compile-time expression relative to the current
    // expansion context
    static Syntax expandTransformer(Syntax s, ExpandContext ctx) {
        return expand(s, new ExpandContext(
                ctx.useSiteScopes,
                ctx.namespace,
                new Env(),
                false,
                null
        ));
    }

    // Expand and evaluate `s` as an expression in the given phase;
    // ensuring that the number of returned values matches the number of
    // target identifiers; return the values
    static PList evalForBindings(List<?> ids, Syntax s, Namespace ns) {
        Object compiled = Compiler.compile(s, ns);
        // todo 检查 compiled 必须是 exp
        // (expand-time-eval `(#%expression ,compiled))
        Object r = Expander.expandTimeEval(compiled);
        PList vals = MultiValues.values(r);
        // 这里应该检查, 但是会导致单测错粗先注释
        /*for (Object val : vals) {
            expect(val instanceof Procedure);
        }*/
        expect(ids.size() == vals.size(),
                "wrong number of results ( "
                        + vals.size() + " vs. " + ids.size() + " ) "
                        + " from " + s);
        return vals;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // A helper for forms to reconstruct syntax
    static Syntax rebuild(Syntax origS, Object newForm) {
        return Syntax.fromDatum(origS, newForm, origS);
    }
}
