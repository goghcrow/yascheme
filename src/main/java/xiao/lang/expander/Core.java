package xiao.lang.expander;

import java.util.HashMap;
import java.util.Map;

import static xiao.lang.Misc.Nullable;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.Values.Callable;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.Bindings.CoreForm;
import static xiao.lang.expander.Bindings.TopLevelBinding;
import static xiao.lang.expander.Utils.tryMatch;

/**
 * core syntax and primitives
 * @author chuxiaofeng
 */
public class Core {

    // Accumulate all core bindings in `core-scope`, so we can
    // easily generate a reference to a core form using `core-stx`:
    final static Scope coreScope = Scope.of();
    final static Syntax coreStx = Scope.add(Syntax.empty(), coreScope);

    // Accumulate added core forms and primitives:
    final static Map<Symbol, SyntaxTransformer> coreForms = new HashMap<>();
    final static Map<Symbol, Callable> corePrimitives = new HashMap<>();

    static void addCoreForm(Symbol sym, SyntaxTransformer proc) {
        addCoreBinding(sym);
        coreForms.put(sym, proc);
    }

    static void addCorePrimitive(Symbol sym, Callable val) {
        addCoreBinding(sym);
        corePrimitives.put(sym, val);
    }

    static void addCoreBinding(Symbol sym) {
        Scope.bind(Syntax.fromDatum(coreStx, sym), new TopLevelBinding(sym));
    }

    // Used only after filling in all core forms and primitives:
    static void declareCoreTopLevel(Namespace ns) {
        corePrimitives.forEach(ns::setVariable);
        coreForms.forEach((sym, proc) -> {
            ns.setTransformer(sym, new CoreForm(proc));
        });
    }

    // Helper for recognizing and dispatching on core forms:
    @Nullable
    static Symbol coreFormSym(Syntax s) {
        Finder r = tryMatch("(id . _)", s);
        if (r != null) {
            Syntax id = r.get("id");
            Binding b = Scope.resolve(id);
            if (b instanceof TopLevelBinding) {
                return b.symbol();
            }
        }
        return null;
    }
}
