package xiao.lang.expander;

import xiao.lang.Env;
import xiao.lang.RT;
import xiao.lang.Values;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.RT.list;
import static xiao.lang.RT.sym;
import static xiao.lang.Values.*;

/**
 * @author chuxiaofeng
 */
public interface CorePrimitives {

    // (syntax? v) → boolean?
    //  v : any/c
    static void isSyntax(Object[] args, Env E, Ctx K) {
        expect(args.length == 1, "arity mismatch");
        K.apply(args[0] instanceof Syntax);
    }

    // (identifier? v) → boolean?
    //  v : any/c
    static void isIdentifier(Object[] args, Env E, Ctx K) {
        expect(args.length == 1, "arity mismatch");
        K.apply(Syntax.isIdentifier(args[0]));
    }

    // (syntax-e stx) — takes a syntax value and returns the value it “wraps”.
    // For example, if stx is an identifier you’d get a symbol,
    // and if it’s a number you’d get the number.
    // If it’s a simple parenthesized form, you’d get a list of syntax values for the subforms.
    // Note that the list can be improper, with the last element being a syntax object
    // that contains a proper list. (But the list will actually be improper if the original syntax was a dotted list.)
    // (syntax-e stx) → any/c
    //  stx : syntax?
    static void syntax_e(Object[] args, Env E, Ctx K) {
        String sig = "(syntax-e stx)";
        expect(args.length == 1, sig);
        expect(args[0] instanceof Syntax, sig);
        Object e = ((Syntax) args[0]).e;
        K.apply(e);
    }

    // https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
    // A common way to “break hygiene” and create a binding that is visible to the macro user’s code is:
    //      (datum->syntax stx 'foo)
    // where stx is some syntax value that you get from the user input to the macro.
    // It returns a foo identifier that has the same lexical context information as stx,
    // so it’s as if it came from there.

    // To create a piece of syntax you use datum->syntax, and you give it an S-expression
    // which will be the “contents” of the resulting syntax object.
    // (The input can contain syntax values.) But when you do that you need to give it
    // the other bits — including the lexical context thing, which you have no access to. The way that’s done is:
    //      (datum->syntax context-stx input-sexpr)
    // This returns a syntax value that wraps the input-sexpr value, using the lexical scope from context-stx.
    // (datum->syntax stx-c v stx-p) → syntax?
    //  stx-c : (or/c syntax? #f)
    //  v : any/c
    static void datum_to_syntax(Object[] args, Env E, Ctx K) {
        String sig = "(datum->syntax stx-c datum stx-p)";
        expect(args.length == 2 || args.length == 3, "arity mismatch: " + sig);
        Object stxC = args[0];
        // 不能等于, 反序列化多个 false
        if (Boolean.FALSE.equals(stxC)) {
            stxC = null; // 兼容 racket datum->syntax 类型
        }
        expect(stxC == null || stxC instanceof Syntax, sig);
        Syntax stxP = null;
        if (args.length == 3) {
            expect(args[2] == null || args[2] instanceof Syntax, sig);
            stxP = ((Syntax) args[2]);
        }
        Object s = Syntax.fromDatum(((Syntax) stxC), args[1], stxP);
        K.apply(s);
    }

    // (syntax->datum stx) — takes a syntax value and returns the plain S-expression
    // that it holds. This is done by recursive uses of syntax-e.
    // (It would be a simple definition that does what you’d think it should do.)
    // (syntax->datum stx) → any/c
    //  stx : syntax?
    static void syntax_to_datum(Object[] args, Env E, Ctx K) {
        String sig = "(syntax->datum stx)";
        expect(args.length == 1, "arity mismatch: " + sig);
        Object a = args[0];
        expect(a instanceof Syntax, sig);
        Object s = Syntax.toDatum(((Syntax) a));
        K.apply(s);
    }

    // (syntax->list stx) — sometimes you want to pull out the list of syntax values
    // from a given parenthesized syntax, but syntax-e does too little
    // (can still return an improper list) and syntax->datum does too much
    // (gives you back raw S-expressions).
    // syntax->list is a utility function that uses syntax-e as many times
    // as needed to get back a proper list of syntax values.
    // If that’s not possible (if the input syntax was not a proper list),
    // it returns #f, so it serves as a predicate too.
    // (syntax->list stx) → (or/c list? #f)
    //  stx : syntax?
    static void syntax_to_list(Object[] args, Env E, Ctx K) {
        String sig = "(syntax->list stx)";
        expect(args.length == 1, "arity mismatch: " + sig);
        Object a = args[0];
        expect(a instanceof Syntax, sig);
        Object s = Syntax.toList(((Syntax) a));
        K.apply(s);
    }

    // There is also (quote-syntax blah) which creates a quoted syntax,
    // with its lexical source from the place it appears.


    /////////////////////////////////////////////////////////////////////////////////////

    // (free-identifier=? a-id b-id) → boolean?
    //  a-id : identifier?
    //  b-id : identifier?
    static void isFreeIdentifierEquals(Object[] args, Env E, Ctx K) {
        expect(args.length == 2, "arity mismatch");
        expect(Syntax.isIdentifier(args[0]), "contract violation");
        expect(Syntax.isIdentifier(args[1]), "contract violation");
        boolean eq = Binding.isFreeIdentifierEquals(((Syntax) args[0]), ((Syntax) args[1]));
        K.apply(eq);
    }

    // (identifier-binding	id-stx [top-level-symbol?])
    //      top-level-symbol? = #f
    // https://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29
    static void identifierBinding(Object[] args, Env E, Ctx K) {
        expect(args.length == 1 || args.length == 2, "arity mismatch");
        expect(Syntax.isIdentifier(args[0]), "contract violation");
        if (args.length == 2) {
            expect(args[1] instanceof Boolean, "contract violation");
        }
        boolean topLevelSym = args.length == 2 ? ((Boolean) args[1]) : false;
        Syntax id = ((Syntax) args[0]);
        K.apply(Binding.identifierBinding(id, topLevelSym));
    }


    /////////////////////////////////////////////////////////////////////////////////////

    static void namespaceSyntaxIntroduce(Object[] args, Env E, Ctx K) {
        expect(args.length == 1, "arity mismatch");
        Syntax s = Expander.namespaceSyntaxIntroduce(args[0]);
        K.apply(s);
    }

    /////////////////////////////////////////////////////////////////////////////////////

    class CurrentNamespace implements Values.Procedure {
        final Expander expander;

        CurrentNamespace(Expander expander) {
            this.expander = expander;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            expect(args.length == 0, "arity mismatch");
            K.apply(expander.currentNamespace());
        }

        @Override
        public String toString() {
            return "#<procedure:current-namespace>";
        }
    }

    // todo
    // https://docs.racket-lang.org/reference/Namespaces.html
    // make-empty-namespace
    // make-base-namespace
    // namespace?

    class NamespaceVariableValue implements Values.Procedure {
        final Expander expander;

        NamespaceVariableValue(Expander expander) {
            this.expander = expander;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            expect(args.length == 1, "arity mismatch");
            expect(args[0] instanceof Symbol, "contract violation");
            Object fail = new Object();
            Namespace ns = expander.currentNamespace();
            Object var = ns.getVariable(((Symbol) args[0]), fail);
            expect(var != fail, "var not found");
            K.apply(var);
        }

        @Override
        public String toString() {
            return "#<procedure:namespace-variable-value>";
        }
    }

    // (namespace-set-variable-value! sym val)
    class NamespaceSetVariableValue implements Values.Procedure {
        final Expander expander;

        NamespaceSetVariableValue(Expander expander) {
            this.expander = expander;
        }

        @Override
        public void call(Object[] args, Env E, Ctx K) {
            expect(args.length == 2, "arity mismatch");
            expect(args[0] instanceof Symbol, "contract violation");

            Symbol sym = (Symbol) args[0];
            Object val = args[1];

            // hack!!! 目前被 hack 实现为通过 Namespace 和 Env 跨编译单元共享数据
            // step 1 + 2 相当于运行时动态的添加 addCorePrimitive
            // 被添加的 procedure, 宏 transformer 中可用
            // step1 expansion 中使用
            // step2 加进去 compile 中 compile-id 会使用
            // todo 这里应该加到 local 而不是 core
            Core.addCoreBinding(sym); // step 1
            Namespace ns = expander.currentNamespace();
            if (val instanceof Procedure) {
                // ss 中定义的 procedure 是没有名字的, 这里加上名字
                Procedure procedure = Procedure.nameOf(sym.name, ((Procedure) val));
                ns.setVariable(sym, procedure); // step 2
            } else {
                assert !(val instanceof Values.Syntax);
                // compileIdentifier 会用到, 取出来会拼接到编译完的代码中去, 不是数据, 所以需要套个 quote
                PList quoteVal = list(sym("quote"), val);
                ns.setVariable(sym, quoteVal); // step 2
            }

            if (Expander.COMPILE_CORE_TO_SYMBOL) {
                expander.expandTImeEnv.put(sym.name, val);
            }

            K.apply(RT.Void());
        }

        @Override
        public String toString() {
            return "#<procedure:namespace-set-variable-value!>";
        }
    }

}
