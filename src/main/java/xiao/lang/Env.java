package xiao.lang;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiConsumer;

import static xiao.lang.Misc.Nullable;

/**
 * Scope
 * @author chuxiaofeng
 */
public class Env {

    // 1. binding 容易区分是未定义还是 null
    // 2. 且可以根据 binding 做一些 feature, 比如 const\freeze 之类
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

    static class ImmutableEnv extends Env {
        ImmutableEnv(Env env) {
            super(env.env, env.parent);
        }

        @Override
        public void put(String id, Object val) {
            throw new UnsupportedOperationException();
        }
    }

    final Map<String, Binding> env;
    final /*@Nullable*/ Env parent;

    public Env() {
        this(null);
    }

    Env(Env parent) {
        this(new LinkedHashMap<>(), parent);
    }

    Env(Map<String, Binding> env, Env parent) {
        this.env = env;
        this.parent = parent;
    }

    // inherit
    public Env derive() {
        return new Env(this);
    }

    public Env immutable() {
        return new ImmutableEnv(this);
    }

    public void forEach(BiConsumer<String, Object> action) {
        env.forEach((k, v) -> action.accept(k, v.value));
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

    public void put(String id, Object val) {
        env.put(id, new Binding(val));
    }

    @Override
    public String toString() {
        return env.toString();
    }
}
