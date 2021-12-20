package xiao.lang2.expander2;

import xiao.lang2.InterpError;

import java.util.Objects;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Procedures.Closure;
import static xiao.lang2.Values.Symbol;
import static xiao.lang2.expander2.Utils.gensym;

/**
 * @author chuxiaofeng
 */
public class Bindings {

    // `variable` is a token to represent a binding to a run-time variable
    final static Symbol variable = gensym("variable");

    public static boolean isVariable(Object a) {
        return a == variable;
    }

    // 两种编译期值, 实际都是 syntax transformer
    // - syntax transformer : xiao.lang.Values.Closure
    //      - 用户代码自定义 transformer
    //      - a procedure of one argument, accept a syntax object and return a syntax object
    //      - https://docs.racket-lang.org/reference/syntax-model.html#%28tech._syntax._transformer%29
    //      - e.g. let-syntax 定义的 (lambda (stx) ...)
    //
    // - CoreForm : xiao.lang.expander2.SyntaxTransformer
    //      - 内置的 core form transformer
    //      - Syntax transform(Syntax s, Env env)
    //      - e.g. lambda, let-syntax, ...

    // A subset of compile-time values are macro transformers
    public static boolean isTransformer(Object a) {
        if (a instanceof Closure) {
            Closure c = (Closure) a;
            return c.params.size() == 1 && c.rest == null;
        }
        return false;
    }

    public static boolean isCoreForm(Object a) {
        return a instanceof CoreForm;
    }

    // A subset of compile-time values are primitive forms
    static class CoreForm {
        final SyntaxTransformer expander;

        CoreForm(SyntaxTransformer expander) {
            this.expander = expander;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // add-local-binding
    static Symbol addLocalBinding(Syntax id) {
        expect(Syntax.isIdentifier(id));
        Symbol key = gensym(((Symbol) id.e).name);
        Scope.bind(id, new LocalBinding(key));
        return key;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // The only non-local bindings are the core forms and primitives:
    static class TopLevelBinding implements Binding {
        final Symbol sym;

        TopLevelBinding(Symbol sym) {
            this.sym = sym;
        }

        // Variable | CoreForm
        @Override
        public Object lookup(ExpandContext ctx, Syntax id) {
            // assume a non-form reference is a primitive
            // SyntaxTransformer c = Core.lookupForm(sym);
            // return c == null ? variable : new CoreForm(c);

            // declareCoreTopLevel 中把 core form 已经全部加入到 namespace
            return ctx.namespace.getTransformer(sym, variable);
        }

        @Override
        public Symbol symbol() {
            return sym;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return sym.equals(((TopLevelBinding) o).sym);
        }

        @Override
        public int hashCode() {
            return sym.hashCode();
        }
    }

    // Represent a local binding with a key, where the value of
    // the key is kept in a separate environment. That indirection
    // ensures that a fully expanded program doesn't reference
    // compile-time values from local bindings, but it records that
    // the binding was local.
    static class LocalBinding implements Binding {
        final Symbol key; // from gensym

        LocalBinding(Symbol key) {
            this.key = key;
        }

        // Variable | Syntax Transformer Closure
        @Override
        public Object lookup(ExpandContext ctx, Syntax id) {
            try {
                // 只有 localBinding 使用 env
                return ctx.env.lookup(key.name);
            } catch (InterpError e) {
                throw new InterpError("identifier used out of context: " + id);
            }
        }

        // A local-binding key is already a symbol
        @Override
        public Symbol symbol() {
            return key;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return key.equals(((LocalBinding) o).key);
        }

        @Override
        public int hashCode() {
            return key.hashCode();
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // Same binding in  sc1  or  sc1 + sc2:
    //(add-binding! (syntax 'a (seteq sc1)) loc/a)
    //
    // Shadowing in sc1 + sc2:
    //(add-binding! (syntax 'b (seteq sc1)) loc/b-out)
    //(add-binding! (syntax 'b (seteq sc1 sc2)) loc/b-in)
    //
    //(check-equal? (resolve (syntax 'a (seteq sc1))) loc/a)
    //(check-equal? (resolve (syntax 'a (seteq sc1 sc2))) loc/a)
    //(check-equal? (resolve (syntax 'b (seteq sc1))) loc/b-out)
    //(check-equal? (resolve (syntax 'b (seteq sc1 sc2))) loc/b-in)
    //
    //(check-equal? (free-identifier=? (syntax 'a (seteq sc1)) (syntax 'a (seteq sc1 sc2))) #t)
    //(check-equal? (free-identifier=? (syntax 'b (seteq sc1)) (syntax 'b (seteq sc1 sc2))) #f)

    // free-identifier=?
    // Determine whether two identifiers have the same binding
    public static boolean isFreeIdentifierEquals(Syntax a, Syntax b) {
        return Objects.equals(Scope.resolve(a), Scope.resolve(b));
    }
}
