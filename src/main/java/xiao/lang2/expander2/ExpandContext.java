package xiao.lang2.expander2;

import xiao.lang2.Env;

import static xiao.lang2.Misc.Nullable;

/**
 * Expansion Context
 *
 * https://docs.racket-lang.org/reference/syntax-model.html#%28tech._context%29
 *
 * Each expansion step occurs in a particular context,
 * and transformers and core syntactic forms may expand differently for different contexts.
 *
 * - top-level context : outside any module, definition, or expression,
 *      except that sub-expressions of a top-level begin form are also expanded as top-level forms.
 *      (top-level 的 begin 会做 splicing 展开)
 * - module-begin context : inside the body of a module, as the only form within the module.
 * - module context : in the body of a module (inside the module-begin layer).
 * - internal-definition context : in a nested context that allows both definitions and expressions.
 * - expression context : in a context where only expressions are allowed.
 *
 * Different core syntactic forms parse sub-forms using different contexts.
 * For example, a let form always parses the right-hand expressions of a binding in an expression context,
 * but it starts parsing the body in an internal-definition context.
 *
 * @author chuxiaofeng
 */
public class ExpandContext {
    @Nullable ScopeSet useSiteScopes; // #f or boxed list: scopes that should be pruned from binders

    // namespace for top-levels binding
    // identifier 从 ns 中解析
    // expansion Namespace::getTransformer
    //      SyntaxTransformer
    // compile   Namespace::getVariable
    //      Compiler::compileIdentifier 用来从 id 索引 core primitive
    final Namespace namespace;

    // 展开期独立的环境, 用来存储 local binding 的值, 保证完全展开的程序不会引用编译期的值
    // environment for local bindings
    // An expansion environment maps a binding to either
    //  - the constant `variable` : for a run-time variable
    //  - a macro-transformer function : a local-binding gensym to a procedure for a macro
    // 展开 lambda body
    // 展开 let[rec]-[syntaxes+]values
    // 展开 define-values define-syntaxes
    final Env env;

    final boolean onlyImmediate; // #t => stop at core forms
    final @Nullable Scope postExpansionScope; // scope to add to every expansion; #f if none

    ExpandContext(ScopeSet useSiteScopes,
                  Namespace namespace,
                  Env env,
                  boolean onlyImmediate,
                  Scope postExpansionScope) {
        this.useSiteScopes = useSiteScopes;
        this.namespace = namespace;
        this.env = env;
        this.onlyImmediate = onlyImmediate;
        this.postExpansionScope = postExpansionScope;
    }

    ExpandContext newEnv(Env env) {
        return new ExpandContext(
                useSiteScopes,
                namespace,
                env,
                onlyImmediate,
                postExpansionScope
        );
    }

    static ExpandContext makeExpandContext(Namespace ns) {
        return new ExpandContext(
                null,
                ns,
                new Env(),
                false,
                null
        );
    }
}
