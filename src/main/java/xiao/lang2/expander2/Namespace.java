package xiao.lang2.expander2;

import java.util.HashMap;
import java.util.Map;

import static xiao.lang2.Values.Symbol;

/**
 * @author chuxiaofeng
 */
public class Namespace {

    // symbol -> core primitives
    Map<Symbol, Object> variables = new HashMap<>(); // sym -> val

    // 'Variable | Bindings.CoreForm | ...
    Map<Symbol, Object/*CompileTime-Value*/> transformers = new HashMap<>(); // sym -> val

    static Namespace currentNamespace = makeEmptyNamespace();

    public static Namespace currentNamespace() {
        return currentNamespace;
    }

    public static Namespace makeEmptyNamespace() {
        return new Namespace();
    }

    @SuppressWarnings("SameParameterValue")
    public Object getVariable(Symbol name, Object failK) {
        return variables.getOrDefault(name, failK);
    }

    public void setVariable(Symbol name, Object val) {
        variables.put(name, val);
    }

    // for top level binding
    @SuppressWarnings("SameParameterValue")
    public Object getTransformer(Symbol name, Object failK) {
        return transformers.getOrDefault(name, failK);
    }

    public void setTransformer(Symbol name, Object val) {
        transformers.put(name, val);
    }

}
