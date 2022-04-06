package xiao.lang.expander;

import xiao.lang.Env;
import xiao.lang.Interp;
import xiao.lang.Values;

import java.util.HashMap;
import java.util.Map;

import static xiao.lang.Contract.expect;
import static xiao.lang.RT.sym;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.CorePrimitives.*;

/**
 * @author chuxiaofeng
 */
public class Namespace {

    // symbol -> core primitives
    // 补充通过 provide 动态加入的 procedure 和 其他 value
    Map<Symbol, Object> variables = new HashMap<>(); // sym -> val

    // 'Variable | Bindings.CoreForm | ...
    Map<Symbol, CoreForm/*CompileTime-Value*/> transformers = new HashMap<>(); // sym -> val

    public static Namespace makeEmptyNamespace() {
        return new Namespace();
    }

    public Env toEnv(Env env) {
        variables.forEach((k, v) -> env.put(k.name, v));
        return env;
    }

    public Object getVariable(Symbol name, Object failK) {
        return variables.getOrDefault(name, failK);
    }

    public void setVariable(Symbol name, Object val) {
        variables.put(name, val);
    }

    // for top level binding
    public CoreForm getTransformer(Symbol name) {
        return transformers.get(name);
    }

    public void setTransformer(Symbol name, CoreForm val) {
        transformers.put(name, val);
    }

    @Override
    public String toString() {
        return "#<namespace>";
    }
}
