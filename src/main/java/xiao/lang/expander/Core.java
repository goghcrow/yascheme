package xiao.lang.expander;

import java.util.HashMap;
import java.util.Map;

import static xiao.lang.Misc.Nullable;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.Values.Procedure;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.Binding.TopLevelBinding;
import static xiao.lang.expander.SyntaxMatcher.tryMatch;

/**
 * core syntax and primitives
 * @author chuxiaofeng
 */
public class Core {

    // 所有 TopLevelBinding (core binding) 都通过 coreStx 加到了 coreScope 中
    // 方便通过 coreStx 来引用 core-form 和 core-primitive
    // Accumulate all core bindings in `core-scope`, so we can
    // easily generate a reference to a core form using `core-stx`:
    // 在 top-level definition context
    // core-scope 充当 outside-edge scope, 所有 top-level 共享的 scope
    final static Scope coreScope = Scope.of(Scope.Type.module);
    final static Syntax coreStx = Scope.add(Syntax.empty(), coreScope);

    static void addCoreBinding(Symbol sym) {
        Scope.bind(Syntax.fromDatum(coreStx, sym), new TopLevelBinding(sym));
    }

    //////////////////////////////////////////////////////////////////////////

    // Accumulate added core forms and primitives:
    final Map<Symbol, SyntaxTransformer> coreForms = new HashMap<>();
    final Map<Symbol, Procedure> corePrimitives = new HashMap<>();

    void addCoreForm(Symbol sym, SyntaxTransformer proc) {
        addCoreBinding(sym);
        coreForms.put(sym, proc);
    }

    void addCorePrimitive(Symbol sym, Procedure val) {
        addCoreBinding(sym);
        corePrimitives.put(sym, val);
    }

    // Used only after filling in all core forms and primitives:
    void declareCoreTopLevel(Namespace ns) {
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
