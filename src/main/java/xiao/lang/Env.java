package xiao.lang;

import java.util.LinkedHashMap;
import java.util.Map;

import static xiao.lang.Misc.Nullable;

/**
 * Scope
 * @author chuxiaofeng
 */
public class Env {

    // binding 容易区分是未定义还是 null, 且可以根据 binding 做一些 feature
    public static class Binding {
        public final Object value;

        Binding(Object value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return value + "";
        }
    }

    public final Map<String, Binding> env = new LinkedHashMap<>();
    public final /*@Nullable*/ Env parent;

    public Env() {
        this(null);
    }

    public Env(Env parent) {
        this.parent = parent;
    }

    // inherit
    public Env derive() {
        return new Env(this);
    }

    public int size() {
        return env.size();
    }

    public Binding lookupLocal(String id) {
        return env.get(id);
    }

    public Object lookup(String id) {
        Binding b = env.get(id);
        if (b != null) {
            return b.value;
        } else if (parent != null) {
            return parent.lookup(id);
        } else {
            throw new InterpError(id + " 未定义");
        }
    }

    public @Nullable Env findDefinedScope(String id) {
        Binding b = env.get(id);
        if (b != null) {
            return this;
        } else if (parent != null) {
            return parent.findDefinedScope(id);
        } else {
            return null;
        }
    }

    public boolean defined(String id) {
        return findDefinedScope(id) != null;
    }

    public void put(String id, Object val) {
        env.put(id, new Binding(val));
    }

    @Override
    public String toString() {
        return env.toString();
    }
}
