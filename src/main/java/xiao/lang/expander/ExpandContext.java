package xiao.lang.expander;

import xiao.lang.Env;

import static xiao.lang.Misc.Nullable;

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

    final static Type root = new Type.Root(null);
    final static Type body = new Type.Body(null);
    final static Type fin_body = new Type.FinBody(null);
    final static Type transformer = new Type.Transformer(null);

    // for debug
    static class Type {
        static class Root extends Type {
            Root(Type parent) {
                super("root", parent);
            }

            @Override
            public String toString() {
                return "root";
            }
        }
        static class Body extends Type {
            Body(Type parent) {
                super("body", parent);
            }
        }
        static class FinBody extends Type {
            FinBody(Type parent) {
                super("fin-body", parent);
            }
        }
        static class Transformer extends Type {
            Transformer(Type parent) {
                super("transformer", parent);}
        }

        final String name;
        final Type parent;
        Type(String name, Type parent) {
            this.name = name;
            this.parent = parent;
        }
        String name() {
            return parent == null ? name : parent.name() + ":" + name;
        }
    }

    final Type type;

    @Override
    public String toString() {
        return "<expand-ctx: " + type.name() + ">";
    }

    final Expander expander;

    // #f or boxed list: scopes that should be pruned from binders
    @Nullable ScopeSet useSiteScopes;

    // namespace for top-levels binding
    // identifier 从 ns 中解析
    // expansion阶段: Namespace::getTransformer
    //      SyntaxTransformer
    // compile阶段:   Namespace::getVariable
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

    // list of scopes that should be pruned by `quote-syntax`
    // expandBody 加入 inside-scope
    // finishExpandingBody 加入 use-site-scope
    // makeLambdaExpander 加入的 lambda-scope
    final ScopeSet scopes;

    // #t => stop at core forms
    final boolean onlyImmediate;

    // scope to add to every expansion; #f if none
    final @Nullable Scope postExpansionScope;

    ExpandContext(Type type,
                  Expander expander,
                  ScopeSet useSiteScopes,
                  Namespace namespace,
                  Env env,
                  ScopeSet scopes,
                  boolean onlyImmediate,
                  Scope postExpansionScope) {
        this.type = type;
        this.expander = expander;
        this.useSiteScopes = useSiteScopes;
        this.namespace = namespace;
        this.env = env;
        this.scopes = scopes;
        this.onlyImmediate = onlyImmediate;
        this.postExpansionScope = postExpansionScope;
    }

//    ExpandContext deriveEnv(Type type) {
//        return deriveEnv(type, scopes);
//    }

    ExpandContext deriveEnv(Type type, ScopeSet scopes) {
        return new ExpandContext(
                type,
                expander,
                useSiteScopes,
                namespace,
                env.derive(),
                scopes,
                onlyImmediate,
                postExpansionScope
        );
    }

    static ExpandContext makeExpandContext(Expander expander, Namespace ns) {
        return new ExpandContext(
                root,
                expander,
                null,
                ns,
                new Env(),
                ScopeSet.empty(),
                false,
                null
        );
    }
}
