package xiao.lang.expander;

import xiao.lang.InterpError;

import java.io.Serializable;
import java.util.Objects;

import static xiao.lang.Contract.expect;
import static xiao.lang.RT.*;
import static xiao.lang.Values.Symbol;

/**
 * 两种 Binding
 * 1. TopLevelBinding =  symbol, core form or primitives
 * 2. LocalBinding = gensym, local binding
 *
 * @author chuxiaofeng
 */
public /*sealed*/ interface Binding {

    /**
     * lookup 返回 'variable 或者 编译期值
     * 1. Bindings.variable
     *      - Binding 或者 运行时变量
     *      - 使用 Bindings.isVariable 判断
     * 2. CoreForm :: (Syntax, ExpandContext) -> Syntax
     *      - xiao.lang.expander.SyntaxTransformer
     *      - 编译期值
     *      - java 代码, 内置的 syntax transformer, 参见: CoreForms.java
     *      - 使用 Bindings.isCoreForm 判断
     *      - e.g. lambda, letrec-syntaxes+values, ...
     * 3. ClosureTransformer :: Syntax -> Syntax
     *      - xiao.lang.expander.ClosureTransformer (lambda (stx) stx)
     *      - 编译期值
     *      - macro transformers
     *      - ss 代码, 使用 define-syntaxes 等编写的自定义 syntax transformer
     *      - 使用 Bindings.isTransformer 判断
     *      - a procedure of one argument, accept a syntax object and return a syntax object
     *      - https://docs.racket-lang.org/reference/syntax-model.html#%28tech._syntax._transformer%29
     *      - e.g. (lambda (stx) ...)
     * 2 和 3 是 两种 syntax transformer (a.k.a. macro), 都是编译期值
     */
    Object lookup(ExpandContext ctx, Syntax id);

    Symbol symbol();

    /////////////////////////////////////////////////////////////////////////////////////////////

    Symbol variable = gensym("variable");

    static boolean isVariable(Object a) {
        return a == variable;
    }

    static boolean isCoreForm(Object a) {
        return a instanceof CoreForm;
    }

    static boolean isTransformer(Object a) {
        return a instanceof ClosureTransformer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // add-local-binding
    static Symbol addLocalBinding(Syntax id) {
        expect(Syntax.isIdentifier(id));
        Symbol key = gensym(((Symbol) id.e).name);
        Scope.bind(id, new LocalBinding(key));
        return key;
    }

    /**
     * free-identifier=?
     * 判断两个 identifiers 是否在是相同的 Binding
     *
     *  e.g.
     * Same binding in  sc1  or  sc1 + sc2:
     * (add-binding! (syntax 'a (seteq sc1)) loc/a)
     *
     * Shadowing in sc1 + sc2:
     * (add-binding! (syntax 'b (seteq sc1)) loc/b-out)
     * (add-binding! (syntax 'b (seteq sc1 sc2)) loc/b-in)
     *
     * (check-equal? (resolve (syntax 'a (seteq sc1))) loc/a)
     * (check-equal? (resolve (syntax 'a (seteq sc1 sc2))) loc/a)
     * (check-equal? (resolve (syntax 'b (seteq sc1))) loc/b-out)
     * (check-equal? (resolve (syntax 'b (seteq sc1 sc2))) loc/b-in)
     *
     * (check-equal? (free-identifier=? (syntax 'a (seteq sc1)) (syntax 'a (seteq sc1 sc2))) #t)
     * (check-equal? (free-identifier=? (syntax 'b (seteq sc1)) (syntax 'b (seteq sc1 sc2))) #f)
     */
    static boolean isFreeIdentifierEquals(Syntax a, Syntax b) {
        return Objects.equals(Scope.resolve(a), Scope.resolve(b));
    }

    /**
     * 标识符是否是 LocalBinding
     */
    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    static boolean isLocalBinding(Syntax ctx, String name) {
        Syntax id = Syntax.fromDatum(ctx, sym(name));
        Object r = Binding.identifierBinding(id);
        return sym("lexical").equals(r);
    }

    static Object identifierBinding(Syntax id) {
        return identifierBinding(id, false);
    }

    /**
     * 判断标识符的绑定
     * id 未绑定, 返回 false
     * id 指向 LocalBing, 结果是 'lexical
     * id 指向 TopLevelBinding 并且 localLevelSym = true, 返回 (list top-sym)
     * id 指向 TopLevelBinding 并且 localLevelSym = false, 返回 false
     */
    static Object identifierBinding(Syntax id, boolean topLevelSym) {
        Binding b = Scope.resolve(id);
        if (b == null) {
            // unbound
            return false;
        } else if (b instanceof LocalBinding) {
            return sym("lexical");
        } else if (b instanceof TopLevelBinding) {
            if (topLevelSym) {
                return list(b.symbol());
            } else {
                return false;
            }
        } else {
            throw new IllegalStateException();
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // core bindings
    // The only non-local bindings are the core forms and primitives
    // TopLevelBinding 是通过 Core.addCoreBinding 加到了 Core.coreStx 上
    class TopLevelBinding implements Binding, Serializable {
        final Symbol sym;

        TopLevelBinding(Symbol sym) {
            this.sym = sym;
        }

        // Bindings.variable | CoreForm
        @Override
        public Object lookup(ExpandContext ctx, Syntax id) {
            // assume a non-form reference is a primitive
            // variable 代表 core-primitive
            // declareCoreTopLevel 中把 core form 已经全部加入到 namespace
            Object/*CoreForm*/ coreForm = ctx.namespace.getTransformer(sym);
            return coreForm == null ? variable : coreForm;
        }

        @Override
        public Symbol symbol() {
            return sym;
        }

        @Override
        public String toString() {
            return "#<top-level-binding:" + sym + ">";
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
    class LocalBinding implements Binding, Serializable {
        final Symbol key; // from gensym

        LocalBinding(Symbol key) {
            this.key = key;
        }

        // Bindings.variable | ClosureTransformer
        @Override
        public Object lookup(ExpandContext ctx, Syntax id) {
            try {
                return ctx.env.lookup(key.name);
            } catch (InterpError e) {
                throw new InterpError("identifier used out of context: " + id);
            }
        }

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
}
