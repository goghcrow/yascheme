package xiao.lang.misc;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public abstract class TypeRef<T> implements Comparable<TypeRef<T>> {
    public final Type type;

    protected TypeRef() {
        Type superClass = getClass().getGenericSuperclass();
        type = ((ParameterizedType) superClass).getActualTypeArguments()[0];
    }

    @Override
    public int compareTo(TypeRef<T> o) {
        return 0;
    }
}
