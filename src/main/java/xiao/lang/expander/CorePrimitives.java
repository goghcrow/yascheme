package xiao.lang.expander;

import xiao.lang.Env;
import xiao.lang.Procedures;
import xiao.lang.Values;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Procedures.list;
import static xiao.lang.Procedures.sym;

/**
 * @author chuxiaofeng
 */
public class CorePrimitives {

    static void datum_to_syntax(Object[] args, Env E, Ctx K) {
        String sig = "(datum->syntax stx-c datum stx-p)";
        expect(args.length == 2 || args.length == 3, sig);
        Object stxC = args[0];
        if (stxC == Boolean.FALSE) {
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

    static void syntax_to_datum(Object[] args, Env E, Ctx K) {
        String sig = "(syntax->datum stx)";
        expect(args.length == 1, sig);
        Object a = args[0];
        expect(a instanceof Syntax, sig);
        Object s = Syntax.toDatum(((Syntax) a));
        K.apply(s);
    }

    static void syntax_e(Object[] args, Env E, Ctx K) {
        String sig = "(syntax-e syntax-object)";
        expect(args.length == 1, sig);
        expect(args[0] instanceof Syntax, sig);
        Object e = ((Syntax) args[0]).e;
        K.apply(e);
    }

    // todo
    static void namespace_set_variable_value(Object[] args, Env E, Ctx K) {
        expect(args.length == 2, "arity mismatch");
        expect(args[0] instanceof Values.Symbol, "contract violation");

        Values.Symbol sym = (Values.Symbol) args[0];
        Object val = args[1];
        Core.addCoreBinding(sym);
        if (val instanceof Values.Callable) {
            Namespace.currentNamespace().setVariable(sym,
                    Values.Callable.nameOf(sym.name, ((Values.Callable) val)));
        } else {
            // compileIdentifier 会用到, 取出来会拼接到编译完的代码中去, 不是数据, 所以需要套个 quote
            Namespace.currentNamespace().setVariable(sym, list(sym("quote"), val));
        }
        Expander.env_forCurrentNamespace.put(sym.name, val);
        K.apply(Procedures.Void());
    }
}
