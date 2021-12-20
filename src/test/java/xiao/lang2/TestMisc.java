package xiao.lang2;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public interface TestMisc {

    static String resource(String path) {
        try {
            URL url = Test.class.getResource(path);
            if (url == null) {
                throw new RuntimeException("文件不存在: " + path);
            }
            return new String(Files.readAllBytes(Paths.get(url.toURI())));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static void expectEquals(Object a, Object b, String ...msg) {
        if (!Objects.equals(a, b)) {
            throw new IllegalStateException(Arrays.toString(msg));
        }
    }

    static void expectTrue(boolean r, String ...msg) {
        if (!r) {
            throw new IllegalStateException(Arrays.toString(msg));
        }
    }


    @SafeVarargs
    static <E> List<E> lists(E... els) {
        if (els.length == 0) {
            return Collections.emptyList();
        } else if (els.length == 1) {
            return Collections.singletonList(els[0]);
        } else {
            List<E> lst = new ArrayList<>();
            Collections.addAll(lst, els);
            return lst;
        }
    }

    @SafeVarargs
    static <E> Set<E> sets(E... els) {
        if (els.length == 0) {
            return Collections.emptySet();
        } else {
            Set<E> set = new HashSet<>();
            Collections.addAll(set, els);
            return set;
        }
    }

    static <E> Set<E> sets(List<E> lst) {
        return new HashSet<>(lst);
    }
}
