package xiao.lang2;

import java.io.IOException;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * @author chuxiaofeng
 */
public interface Misc {

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
    @interface Nullable { }

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
    @interface NotNull { }

    abstract class TypeRef<T> implements Comparable<TypeRef<T>> {
        public final Type type;
        protected TypeRef() {
            Type superClass = getClass().getGenericSuperclass();
            type = ((ParameterizedType) superClass).getActualTypeArguments()[0];
        }
        @Override
        public int compareTo(TypeRef<T> o) { return 0; }
    }

    static <T extends Throwable> void sneakyThrows(Throwable t) throws T {
        if (t instanceof InvocationTargetException) {
            sneakyThrows(((InvocationTargetException) t).getTargetException());
        } else {
            //noinspection unchecked
            throw ((T) t);
        }
    }

    static Class<?> classOf(String klass) {
        try {
            return Class.forName(klass);
        } catch (ClassNotFoundException e) {
            sneakyThrows(e);
            return Object.class;
        }
    }

    static String resource(String path) {
        try {
            URL res = Misc.class.getResource(path);
            if (res == null) {
                throw new RuntimeException(path + " not found");
            }
            return new String(Files.readAllBytes(Paths.get(res.toURI())));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static String read(String path) {
        try {
            byte[] bytes = Files.readAllBytes(Paths.get(path));
            ByteBuffer buf = ByteBuffer.wrap(bytes);
            return UTF_8.decode(buf).toString();
        } catch (IOException e) {
            sneakyThrows(e);
            return null;
        }
    }
}
