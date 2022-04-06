package xiao.lang;

import java.io.*;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * @author chuxiaofeng
 */
public interface Misc {

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.LOCAL_VARIABLE})
    @interface Nullable { }

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

    static Path pathOfRes(String resPath) {
        try {
            URL res = Misc.class.getResource(resPath);
            if (res == null) {
                throw new RuntimeException(resPath + " not found");
            }
            return Paths.get(res.toURI());
        } catch (Exception e) {
            sneakyThrows(e);
            return Paths.get("");
        }
    }

    static String resource(String resPath) {
        return read(pathOfRes(resPath));
    }

    static String read(String path) {
        return read(Paths.get(path));
    }

    static String read(Path path) {
        try {
            byte[] bytes = Files.readAllBytes(path);
            ByteBuffer buf = ByteBuffer.wrap(bytes);
            return UTF_8.decode(buf).toString();
        } catch (IOException e) {
            sneakyThrows(e);
            return "";
        }
    }

    static String md5(String s) {
        try {
            MessageDigest m = MessageDigest.getInstance("MD5");
            m.update(s.getBytes());
            byte[] digest = m.digest();
            return printHexBinary(digest).toUpperCase();
        } catch (NoSuchAlgorithmException e) {
            sneakyThrows(e);
            return null;
        }
    }

    char[] hexCode = "0123456789ABCDEF".toCharArray();
    static String printHexBinary(byte[] data) {
        StringBuilder r = new StringBuilder(data.length * 2);
        for (byte b : data) {
            r.append(hexCode[(b >> 4) & 0xF]);
            r.append(hexCode[(b & 0xF)]);
        }
        return r.toString();
    }

    static void serialize(String path, Object o) {
        try (FileOutputStream fos = new FileOutputStream(path);
             ObjectOutputStream oos = new ObjectOutputStream(fos)
        ) {
            oos.writeObject(o);
            oos.flush();
        } catch (Exception e) {
            try {
                Files.deleteIfExists(Paths.get(path));
            } catch (IOException ignored) { }
            sneakyThrows(e);
        }
    }

    static <T> T unSerialize(String path) {
        try (FileInputStream fis = new FileInputStream(path);
             ObjectInputStream ois = new ObjectInputStream(fis)
        ) {
            //noinspection unchecked
            return (T) ois.readObject();
        } catch (Exception e) {
            sneakyThrows(e);
            return null;
        }
    }
}
