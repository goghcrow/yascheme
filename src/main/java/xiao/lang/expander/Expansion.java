package xiao.lang.expander;

import xiao.lang.*;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static xiao.lang.Contract.expect;
import static xiao.lang.Names.SLASH;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.Procedures.Closure;
import static xiao.lang.Procedures.MultiValues;
import static xiao.lang.Values.*;
import static xiao.lang.expander.CUtils.map;
import static xiao.lang.expander.SyntaxMatcher.match;

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
        } else if (RT.isPair(s.e) && Syntax.isIdentifier(RT.car(s.e))) {
            // ::step-2::
            // If it is a syntax-object pair whose first element is an identifier,
            // and if the identifier has a binding other than as a top-level variable,
            // then the identifier’s binding is used to continue.
            return expandIdAppForm(((Syntax) RT.car(s.e)), s, ctx);
        } else if (RT.isPair(s.e) || RT.isNull(s.e)) {
            // ::step-3::
            // If it is a syntax-object pair of any other form, then a new syntax-object symbol '#%app is created
            // using the lexical information of the pair with implicit-made-explicit properties.
            // If the resulting #%app identifier has no binding, parsing fails with an exn:fail:syntax exception.
            // Otherwise, the new identifier is combined with the original pair to form a new syntax-object pair
            // (also using the same lexical information as the original pair), and the #%app binding is used to continue.

            // An "application" form that doesn't start with an identifier, so use implicit `#%app`
            // e.g. ((curried '1) '2)
            // isNull(s.e) 分支, () 最终在 expand #%app 时匹配失败 (#%app rator rand ...)
            return expandImplicit(RT.sym("#%app"), s, ctx);
        } else {
            // ::step-4::
            // If it is any other syntax object, then a new syntax-object symbol '#%datum is created
            // using the lexical information of the original syntax object with implicit-made-explicit properties.
            // If the resulting #%datum identifier has no binding, parsing fails with an exn:fail:syntax exception.
            // Otherwise, the new identifier is combined with the original syntax object in a new syntax-object pair
            // (using the same lexical information as the original pair), and the #%datum binding is used to continue.

            // Anything other than an identifier or parens triggers the implicit `#%datum` form
            return expandImplicit(RT.sym("#%datum"), s, ctx);
        }
    }

    static Syntax expandIdentifier(Syntax s, ExpandContext ctx) {
        // If it is an identifier (i.e., a syntax-object symbol),
        // then a binding is determined by the identifier’s lexical information.
        Binding b = Scope.resolve(s);
        if (b == null) {
            return expandUnboundIdentifier(s, ctx);
        } else {
            // If the identifier has a binding, that binding is used to continue.
            // Variable or core-form or form as identifier macro
            Object val = b.lookup(ctx, s);
            return dispatch(val, s, ctx);
        }
    }

    static Syntax expandUnboundIdentifier(Syntax s, ExpandContext ctx) {
//        Symbol sym = (Symbol) s.e;
//        if (Reader.isStaticFieldAccess(sym)) {
//            return expand(rewriteJavaStaticFieldAccess(s, sym.name), ctx);
//        } else {
            // The implicit `#%top` form handles unbound identifiers
            return expandImplicit(RT.sym("#%top"), s, ctx);
//        }
    }

    static Syntax rewriteJavaStaticFieldAccess(Syntax s, String id) {
        // ClassName/staticField ==> (. ClassName staticField)
        int idx = id.indexOf("/");
        String klass = id.substring(0, idx);
        String field = id.substring(idx + 1);
        return rebuild(s, RT.list(
                Syntax.fromDatum(Core.coreStx, RT.sym(".")),
                RT.sym(klass),
                RT.sym(field)
        ));
    }

    // An "application" form that starts with an identifier
    // expand an identifier in "application" position
    // e.g. (lambda (x) x)
    // e.g. (f '1)
    // e.g. (one) ; (let-syntax ([one (lambda (stx) (quote-syntax '1))]) (one))
    static Syntax expandIdAppForm(Syntax id, Syntax s, ExpandContext ctx) {
        Binding binding = Scope.resolve(id);
        if (binding == null) {
            return expandUnboundIdAppForm(id, s, ctx);
        } else {
            // if the identifier has a binding other than as a top-level variable,
            // then the identifier’s binding is used to continue.

            // Find out whether it's bound as a variable, syntax, or core form
            Object t = binding.lookup(ctx, id);

            // debug
            if (binding instanceof Binding.TopLevelBinding) {
                assert Binding.isVariable(t) || t instanceof CoreForm;
            }

            if (Binding.isVariable(t)) {
                // adds `#%app` and expands every application to an `#%app` form
                // https://docs.racket-lang.org/reference/application.html
                // the expander converts this form to (#%app proc-expr arg ...),
                // giving #%app the lexical context that is associated with the original form
                // (i.e., the pair that combines proc-expr and its arguments)

                // Not as syntax or core form, so use implicit `#%app`
                return expandImplicit(RT.sym("#%app"), s, ctx);
            } else {
                // Syntax or core form as "application"
                return dispatch(t, s, ctx);
            }
        }
    }

    static Syntax expandUnboundIdAppForm(Syntax id, Syntax s, ExpandContext ctx) {
        Object car = id.unbox();
        if (isJavaInterop(car)) {
            return expand(rewriteJavaInterop(car, s, ctx), ctx);
        } else {
            // top-level variable
            // The `#%app` binding might do something with unbound ids
            // 处理路径:
            // --> expandImplicit   :: (#%app unbound-id rand ...)
            // --> expandIdentifier :: resolve unbound-id fail, => #%top unbound-id
            // --> expandImplicit   :: resolve #%top fail => no transformer binding for #％top unbound-id
            return expandImplicit(RT.sym("#%app"), s, ctx);
        }
    }

    @SuppressWarnings("RedundantIfStatement")
    static boolean isJavaInterop(Object car) {
        if (Reader.isInstanceMemberAccess(car)) {
            return true;
        } else if (Reader.isStaticMethodCall(car)) {
            return true;
        } else if (Reader.isNewInstance(car)) {
            return true;
        } else {
            return false;
        }
    }

    static Syntax rewriteJavaInterop(Object car, Syntax s, ExpandContext ctx) {
        if (Reader.isInstanceMemberAccess(car)) {
            if (Reader.isInstanceFieldAccess(car)) {
                // (.-instanceField instance-expr) ==> (. instance-expr -instanceField)
                Finder r = match("(id:.-instanceField instance-expr)", s);
                Syntax hyphenField = r.get("id:.-instanceField");
                Syntax expr = r.get("instance-expr");
                Symbol field = RT.sym(((Symbol) hyphenField.e).name.substring(1));

                // 只改写, 不展开, dot form 再展开
                return rebuild(s, RT.list(
                        Syntax.fromDatum(Core.coreStx, RT.sym(".")),
                        expr, // expand(expr, ctx)
                        Syntax.fromDatum(hyphenField, field, hyphenField)
                ));
            } else {
                // (.instanceMember instance-expr args ...) ==> (. instance-expr (instanceMember args ...))
                // (.instanceMember ClassName args ...) ==> (. ClassName (instanceMember args ...))
                Finder r = match("(id:.instanceMember cls-or-ins args ...)", s);
                Syntax dotField = r.get("id:.instanceMember");
                Syntax clsOrIns = r.get("cls-or-ins");
                List<Syntax> args = r.get("args"); // PList
                Symbol field = RT.sym(((Symbol) dotField.e).name.substring(1));

                // 只改写, 不展开, dot 再展开
                // 有参数一定是方法调用, 方法名和参数部分加上 list
                // 无参数可能是0参方法调用或者是属性访问, 先统一加上 list
                // 调用是方法没找到时候再退回读属性
                // @see CallInstanceMethod CallStaticMethod
                return rebuild(s, RT.list(
                        Syntax.fromDatum(Core.coreStx, RT.sym(".")),
                        clsOrIns,
                        RT.list(
                                Syntax.fromDatum(dotField, field, dotField),
                                RT.splice(args) // splice(map(args, arg -> expand(arg, ctx)))
                        )
                ));
            }
        } else if (Reader.isStaticMethodCall(car)) {
            // (ClassName/staticMethod args ...) ==> (. ClassName-symbol (method-symbol args ...))
            Finder r = match("(id:ClassName/staticMethod args ...)", s);
            Syntax clsMethod = r.get("id:ClassName/staticMethod");
            List<Syntax> args = r.get("args"); // PList
            String id = ((Symbol) clsMethod.e).name;
            int idx = id.indexOf(SLASH);
            String klass = id.substring(0, idx);
            String method = id.substring(idx + 1);

            return rebuild(s, RT.list(
                    Syntax.fromDatum(Core.coreStx, RT.sym(".")),
                    Syntax.fromDatum(clsMethod, RT.sym(klass), clsMethod),
                    RT.list(
                            Syntax.fromDatum(clsMethod, RT.sym(method), clsMethod),
                            // 延迟到 CoreForms::dot 在展开, 这里只改写, 统一流程
                            RT.splice(args) // splice(map(args, arg -> expand(arg, ctx)))
                    )
            ));
        } else if (Reader.isNewInstance(car)) {
            // (ClassName. args ...) ==> (new ClassName args ...)
            Finder r = match("(id:ClassName. args ...)", s);
            Syntax clsNameDot = r.get("id:ClassName.");
            String id = ((Symbol) clsNameDot.e).name;
            List<Syntax> args = r.get("args");
            Symbol klass = RT.sym(id.substring(0, id.length() - 1));

            return rebuild(s, RT.list(
                    Syntax.fromDatum(Core.coreStx, RT.sym("new")),
                    Syntax.fromDatum(clsNameDot, klass, clsNameDot),
                    // 延迟到 CoreForms::new1 在展开, 这里只改写, 统一流程
                    RT.splice(args) // splice(map(args, arg -> expand(arg, ctx)))
            ));
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
            if (Binding.isCoreForm(t)) {
                if (ctx.onlyImmediate) {
                    return s;
                } else {
                    Syntax newStxPair = Syntax.fromDatum(s, RT.cons(sym, s), s);
                    return dispatch(t, newStxPair, ctx);
                }
            } else if (t instanceof SyntaxTransformer) {
                Syntax newStxPair = Syntax.fromDatum(s, RT.cons(sym, s), s);
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

        if (Binding.isCoreForm(t)) {
            // 3. A core syntactic form, which is parsed as described for each form in Syntactic Forms.
            // Parsing a core syntactic form typically involves recursive parsing of sub-forms,
            // and may introduce bindings that determine the parsing of sub-forms.
            if (ctx.onlyImmediate) {
                return s;
            } else {
                return ((CoreForm) t).transformer.transform(s, ctx);
            }
        } else if (Binding.isTransformer(t)) {
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
            Closure syntaxTransformer = ((ClosureTransformer) t).closure;
            Syntax transformed = applyTransformer(syntaxTransformer, s, ctx);
            // 这里处理生成宏的宏, 递归宏展开!!!
            return expand(transformed, ctx);
        } else if (Binding.isVariable(t)) {
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

        // https://docs.racket-lang.org/reference/syntax-model.html#(part._transformer-model)
        // Before the expander passes a syntax object to a transformer,
        // the syntax object is extended with a fresh macro-introduction scope
        // (that applies to all sub-syntax objects) to distinguish syntax objects
        // at the macro’s use site from syntax objects that are introduced by the macro;
        //  in the result of the transformer the presence of the scope is flipped,
        //  so that introduced syntax objects retain the scope, and use-site syntax objects do not have it.
        //  In addition, if the use of a transformer is in the same definition context as its binding,
        //  the use-site syntax object is extended with an additional fresh use-site scope
        //  that is not flipped in the transformer’s result,
        //  so that only use-site syntax objects have the use-site scope.
        // The scope-introduction process for macro expansion helps
        // keep binding in an expanded program consistent with the lexical structure of the source program.

        // macro-scope
        // 宏展开引入的scope，通过宏展开前后的两次反转，所有展开过程中新引入的syntax对象都会添加上该scope
        // Scope.flip(transformedS, introScope)
        // 会把展开前的 syntax 引入的 introScope 移除, 展开过程新引入的 syntax 加入 introScope
        // 用来识别新加入的 syntax
        // create a scope to represent the macro step
        Scope introScope = Scope.of(Scope.Type.macro, s);
        // tentatively add the scope to the input
        Syntax introS = Scope.add(s, introScope);
        // use of a transformer is in the same definition context as its binding
        // ExpandContext bodyCtx 才会加 use-site scope
        // In a definition context, we need use-site scopes
        Syntax useS = maybeAddUseSiteScope(introS, ctx);
        // call the transformer
        Syntax transformedS = callTransformer(t, useS);
        // flip intro scope to get final result
        Syntax resultS = Scope.flip(transformedS, introScope);
        // use of a transformer is in the same definition context as its binding
        // ExpandContext bodyCtx 才会处理添加 inside scope
        // In a definition context, we need to add the inside-edge scope to any expansion result
        return maybeAddPostExpansionScope(resultS, ctx);
    }

    static Syntax callTransformer(Procedure p, Syntax stx) {
        Object transformedS = callProcedure(p, stx);
        expect(transformedS instanceof Syntax,
                "transformer produced non-syntax: " + transformedS);
        return ((Syntax) transformedS);
    }

    public /*for only test*/
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
            Scope sc = Scope.of(Scope.Type.use_site, s);
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


    // https://docs.racket-lang.org/guide/define.html
    // 4.5.4 Internal Definitions
    // When the grammar for a syntactic form specifies body, then the corresponding form
    // can be either a definition or an expression.
    // A definition as a body is an internal definition. 🎈
    //
    // Expressions and internal definitions in a body sequence can be mixed,
    // as long as the last body is an expression. 🎈
    // Internal definitions in a particular body sequence are mutually recursive;
    // that is, any definition can refer to any other definition—as long as
    // the reference isn’t actually evaluated before its definition takes place.
    // If a definition is referenced too early, an error occurs.
    //
    // A sequence of internal definitions using just define is easily translated to
    // an equivalent letrec form.
    // However, other definition forms can appear as a body, including define-values,
    // struct (see Programmer-Defined Datatypes) or define-syntax (see Macros).


    // 这里主要处理 internal-definition context (没实现 module context)
    // partial expansion 用来区分 forms 是否是 definition, expression 或者其他 declaration
    // 当相关绑定是原始的语法形式(define-values\define-syntaxes\begin)时, partial expansion 会终止正常的递归展开
    // 通过 ExpandContext 的 onlyImmediate 属性来实现
    // 特例, 当展开过程需要添加 #%app, #%datum, or #%top 标识符时 (参见 expandImplicit 方法), 遇到 CoreForm 时候
    // 也会终止正常的递归展开

    // internal-definition context 支持混合表达式的局部定义
    // internal-definition 中的定义等价于通过 letrec-syntaxes+values 构造的局部定义
    // 宏展开把 internal-definition 转换成 letrec-syntaxes+values

    // 内部定义中序列的每一个 body 的局部展开满足
    // define-values: 添加绑定到表, 定义接下来的展开被推迟, 其余 body 的部分展开继续
    // define-syntaxes:  rhs 展开求值, 添加 transformer 绑定, 其余 body 的部分展开继续
    // begin: begin的子表(可空), 被展开到内部定义序列开头, 其余 body 的部分展开继续
    // 其余 primitive 表达式, 表达式的后续展开被推迟, 其余 body 的部分展开继续
    // 所有 body 都被部分展开完成后, 如果没有发现定义, 表达式依次展开到 begin 中
    // 否则, 定义之后至少要有一个表达式, 定义之间的表达式都要被转换成 (define-values () (begin expr (values))) 形式
    // 定义最后会被转换成 letrec-syntaxes+values 形式, 最后定义末尾的所有表达式
    // 都会成为 letrec-syntaxes+values 的 body

    // 部分展开开始前, 内部定义上下文的所有 body 会加入一个新的 outside-edge scope
    // outside-edge scope 给原始语法对象打上标记
    // inside-edge scope 也会被加入到 body 上, 后续 部分展开的结果也会被被标记
    // (通过 finishExpandingBody 延迟展开部分展开的结果不会标记)
    // (部分展开只包括 CoreForm 和 %xxx, 会推迟展开不打标, transformer展开 会通过 PostExpansionScope 打标)
    // inside-edge scope 保证内部定义引入的所有绑定被打上标


    // Expand a sequence of body forms in a definition context
    // definition context:
    //  - lambda
    //  - let[rec]-[syntaxes+]values
    static Syntax expandBody(List<Syntax> bodys, Scope sc, Syntax s, ExpandContext ctx) {
        // 在 definition context 展开的时候, 输入的 syntax 对象会带上 outside-edge scope 和 inside-edge scope
        // 并且, 展开的结果也会带上 inside-edge scope

        // 取消 outside scope
        // In principle, we have an outside-edge scope that identifies the
        // original content of the definition context --- but a body always
        // exists inside some binding form, so that form's scope will do;
        // the inside-edge scope identifies any form that appears (perhaps
        // through macro expansion) in the definition context

        // outside-edge scope 区分宏引入的 identifier
        //
        // The outside-edge scope identifies the original content of the definition context
        //
        // This outside-edge scope effectively identifies syntax objects that are present in the original form.
//        Scope outsideSc = Scope.of(Scope.Type.intdef, s);

        // inside-edge scope 区分不同的 definition context
        //
        // The inside-edge scope identifiers any form that appears (perhaps
        // through macro expansion) in the definition context
        // (maybeAddPostExpansionScope )
        //
        // This inside-edge scope ensures that all bindings introduced by
        // the internal-definition context have a particular scope in common.

        // In principle, we have an outside-edge scope that identifies the
        // original content of the definition context --- but a body always
        // exists inside some binding form, so that form's scope will do;
        // the inside-edge scope identifies any form that appears (perhaps
        // through macro expansion) in the definition context
        Scope insideSc = Scope.of(Scope.Type.intdef, s);

        // Create an expansion context for expanding only immediate macros;
        // this partial-expansion phase uncovers macro and variable definitions
        // in the definition context
        ExpandContext bodyCtx = new ExpandContext(
                // use-site-scope
                // 当一个宏的定义和使用在同一个 definition context 时，宏的参数会带上该 scope
                ExpandContext.body,
                ctx.expander,
                ScopeSet.empty(), // useSiteScopes!!!
                ctx.namespace,
                ctx.env,
                ctx.scopes/*.add(outsideSc)*/.add(insideSc).add(ctx.scopes),
                true,
                // 展开的结果会带上 inside-edge scope
                insideSc // postExpansionScope!!!
        );

        LinkedList<Syntax> initBodys = new LinkedList<>();
        for (Syntax body : bodys) {
            // 输入的 syntax 对象会带上 outside-edge scope 和 inside-edge scope
            initBodys.add(
                    Scope.add(
//                        Scope.add(
                                Scope.add(
                                        body,
                                        sc // let\lambda 的 local-scope
                                ),
//                                outsideSc
//                        ),
                        insideSc
                    )
            );
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
            // bodyCtx 的 onlyImmediate = true, 把 coreForm 的 expand 转移到这里处理
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

                        PList ids = removeUseSiteScope(RT.listColl(map(idLst, it -> it)), bodyCtx);
                        DupCheck.checkNoDuplicateIds(ids, expBody, dups);

                        // bodyCtx = bodyCtx.deriveEnv();
                        for (Object id : ids) {
                            Symbol key = Binding.addLocalBinding(((Syntax) id));
                            bodyCtx.env.put(key.name, Binding.variable);
                        }

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
                        PList ids = removeUseSiteScope(RT.listColl(map(idLst, it -> it)), bodyCtx);
                        DupCheck.checkNoDuplicateIds(ids, expBody, dups);

                        Syntax rhs = r.get("rhs");
                        List<ClosureTransformer> vals = evalForSyntaxesBinding(rhs, ids, ctx);

                        // bodyCtx = bodyCtx.deriveEnv();
                        for (int i = 0; i < vals.size(); i++) {
                            // !!! 注意, 这里必须用 ids 而不是 idLst, 因为 idLst 多出来了useSiteScope
                            // 会导致后面 addLocalBinding 之后 resolve 不到
                            Syntax id = ((Syntax) ids.get(i));
                            // A transformer, such as introduced by define-syntax or let-syntax.
                            // If the associated value is a procedure of one argument,
                            // the procedure is called as a syntax transformer (described below),
                            ClosureTransformer transformer = vals.get(i);
                            Symbol key = Binding.addLocalBinding(id);
                            bodyCtx.env.put(key.name, transformer);
                        }
                        continue;
                    }
                }
            }

            // Found an expression; accumulate it and continue
            doneBodys.addLast(expBody);
        }

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

        ScopeSet scopes = bodyCtx.scopes;
        if (bodyCtx.useSiteScopes != null) {
            scopes.add(bodyCtx.useSiteScopes);
        }
        // As we finish expanding, we're no longer in a definition context
        ExpandContext finishCtx = new ExpandContext(
                ExpandContext.fin_body,
                bodyCtx.expander,
                null,
                bodyCtx.namespace,
                bodyCtx.env,
                scopes,
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
                    RT.list(
                            Syntax.fromDatum(Core.coreStx, RT.sym("begin")),
                            RT.splice(map(doneBodys, it -> expand(it, finishCtx)))
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
                    RT.list(
                            Syntax.fromDatum(Core.coreStx, RT.sym("letrec-values")),
                            RT.listColl(
                                    map(valBinds, bind -> RT.list(
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

    // racket 把根据引用情况拆分了 letrec 和 let
    // https://github.com/racket/racket/blob/816c26482c414ed7a244bc25d0ba9749a6290e0b/racket/src/expander/expand/body.rkt
    // Roughly, create a `letrec-values` for for the given ids, right-hand sides, and
    // body. While expanding right-hand sides, though, keep track of whether any
    // forward references appear, and if not, generate a `let-values` form, instead,
    // at each binding clause. Similar, end a `letrec-values` form and start a new
    // one if there were forward references up to the clause but not beyond.
    // Returns a single form.

    // Helper to turn an expression into a binding clause with zero bindings
    // (defined-values () (begin <expr> (values)))
    static Bind noBinds(Syntax expr, Syntax s) {
        return new Bind(
                RT.list(),
                Syntax.fromDatum(
                        null,
                        RT.list(
                                Syntax.fromDatum(Core.coreStx, RT.sym("begin")),
                                expr,
                                RT.list(
                                        Syntax.fromDatum(Core.coreStx, RT.sym("#%app")),
                                        Syntax.fromDatum(Core.coreStx, RT.sym("values"))
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
                RT.list(
                        Syntax.fromDatum(Core.coreStx, RT.sym("#%app")),
                        Syntax.fromDatum(Core.coreStx, RT.sym("void"))
                )
        );
    }

    // https://docs.racket-lang.org/reference/syntax-model.html#(part._transformer-model)
    // A use-site scope on a binding identifier is ignored
    // when the definition is in the same context where the use-site scope was introduced.
    //
    // This special treatment of use-site scopes allows a macro to expand to a visible definition.
    // Helper to remove any created use-site scopes from the left-hand
    // side of a definition that was revealed by partial expansion in a
    // definition context
    // definition 与 use-site scope 被加入时的 context 相同时, (展开的 ExpandContext相同)
    // definition 中绑定标识符的 use-site scope 会被忽略
    // 这种 对 use-site scopes 的特殊处理是为了宏中的定义对外可见
    static <T> T removeUseSiteScope(T s, ExpandContext ctx) {
        return Scope.remove(s, ctx.useSiteScopes);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // 注意:
    // 暂时 修改成只用于 define-syntax, letrec-syntaxes+values 定义 transformer 闭包 (lambda (stx) ... )
    // 目前仅有这两个形式可以用来定义 syntax
    // 不用于返回其他编译期值
    // 返回值从 PList 修改成 List<ClosureTransformer>

    // Expand and evaluate `s` as a compile-time expression, returning
    // only the compile-time values
    // ids = List<Syntax>
    static List<ClosureTransformer> evalForSyntaxesBinding(Syntax rhs, List<?> ids, ExpandContext ctx) {
        return expandAndEvalForSyntaxesBinding(rhs, ids, ctx);
    }

    // Expand and evaluate `s` as a compile-time expression, ensuring that
    // the number of returned values matches the number of target
    // identifiers; return the expanded form as well as its values
    static List<ClosureTransformer> expandAndEvalForSyntaxesBinding(Syntax rhs, List<?> ids, ExpandContext ctx) {
        // https://docs.racket-lang.org/reference/syntax-model.html#(part._transformer-model)
        // The value for the binding is obtained by evaluating the expression in the define-syntaxes form.
        // This expression must be expanded (i.e., parsed) before it can be evaluated,
        // and it is expanded at phase level 1 (i.e., in the transformer environment) instead of phase level 0.
        Syntax expRhs = expandTransformer(rhs, ctx);
        return evalForBindings(ids, expRhs, ctx);
    }

    // Expand `s` as a compile-time expression relative to the current
    // expansion context
    static Syntax expandTransformer(Syntax s, ExpandContext ctx) {
        return expand(s, new ExpandContext(
                ExpandContext.transformer,
                ctx.expander,
                ctx.useSiteScopes,
                ctx.namespace,
                new Env(), // !!!
                ScopeSet.empty(),
                false,
                null
        ));
    }

    // Expand and evaluate `s` as an expression in the given phase;
    // ensuring that the number of returned values matches the number of
    // target identifiers; return the values
    static List<ClosureTransformer> evalForBindings(List<?> ids, Syntax s, ExpandContext ctx) {
        Object compiled = Compiler.compile(s, ctx.namespace);
        // todo 检查 compiled 必须是 exp
        // (expand-time-eval `(#%expression ,compiled))
        Object r = ctx.expander.expandTimeEval(compiled);
        PList vals = MultiValues.values(r);

        expect(ids.size() == vals.size(),
                "wrong number of results ( "
                        + vals.size() + " vs. " + ids.size() + " ) "
                        + " from " + s);

        List<ClosureTransformer> transformers = new ArrayList<>(vals.size());
        for (Object val : vals) {
            // (lambda (stx) stx)
            expect(val instanceof Closure, "expect transformer binding, but: " + val);
            Closure c = (Closure) val;
            expect(c.params.size() == 1 && c.rest == null, "illegal transformer, arity mismatch");
            transformers.add(new ClosureTransformer(c));
        }

        return transformers;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // A helper for forms to reconstruct syntax
    static Syntax rebuild(Syntax origS, Object newForm) {
        return Syntax.fromDatum(origS, newForm, origS);
    }
}
