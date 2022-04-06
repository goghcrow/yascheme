package xiao.lang.expander;

import xiao.lang.RT;
import xiao.lang.Values;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.function.*;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.sneakyThrows;

/**
 * Collection Utils
 * @author chuxiaofeng
 */
public class CUtils {

    public interface TriFunction<T1, T2, T3, R> {
        R apply(T1 t1, T2 t2, T3 t3);
    }

    private static <C extends Collection<?>, R extends Collection<?>>
    R newCollection(C t) {
        try {
            //noinspection unchecked
            return ((R) t.getClass().newInstance());
        } catch (InstantiationException | IllegalAccessException e) {
            sneakyThrows(e);
            return null;
        }
    }

    public static <From, To, CFrom extends Collection<From>, CTo extends Collection<To>>
    CTo map(CFrom c1, Function<From, To> mapper) {
        return map(c1, mapper, () -> newCollection(c1));
    }

    public static<
            From1,
            From2,
            To,
            CFrom1 extends Collection<From1>,
            CFrom2 extends Collection<From2>,
            CTo extends Collection<To>>
    CTo map(CFrom1 c1, CFrom2 c2, BiFunction<From1, From2, To> mapper) {
       return map(c1, c2, mapper, () -> newCollection(c1));
    }

    public static<
            From1,
            From2,
            From3,
            To,
            CFrom1 extends Collection<From1>,
            CFrom2 extends Collection<From2>,
            CFrom3 extends Collection<From3>,
            CTo extends Collection<To>>
    CTo map(CFrom1 c1, CFrom2 c2, CFrom3 c3, TriFunction<From1, From2, From3, To> mapper) {
        return map(c1, c2, c3, mapper, () -> newCollection(c1));
    }

    public static <
            From,
            To,
            CFrom extends Collection<From>,
            CTo extends Collection<To>>
    CTo map(CFrom c1, Function<From, To> mapper, Supplier<CTo> newer) {
        CTo r;
        if (c1 instanceof Values.PList) {
            r = ((CTo) new ArrayList<>(c1.size()));
        } else {
            r = newer.get();
        }


        for (From el : c1) {
            To apply = mapper.apply(el);
            r.add(apply);
        }

        if (c1 instanceof Values.PList) {
            return ((CTo) RT.listColl(((ArrayList) r)));
        } else {
            return r;
        }
    }

    public static<
            From1,
            From2,
            To,
            CFrom1 extends Collection<From1>,
            CFrom2 extends Collection<From2>,
            CTo extends Collection<To>>
    CTo map(CFrom1 c1,
            CFrom2 c2,
            BiFunction<From1, From2, To> mapper,
            Supplier<CTo> newer
    ) {
        CTo r;
        if (c1 instanceof Values.PList) {
            r = ((CTo) new ArrayList<>(c1.size()));
        } else {
            r = newer.get();
        }

        Iterator<From1> iter1 = c1.iterator();
        Iterator<From2> iter2 = c2.iterator();
        while (iter1.hasNext()) {
            expect(iter2.hasNext());
            From1 from1 = iter1.next();
            From2 from2 = iter2.next();
            r.add(mapper.apply(from1, from2));
        }
        expect(!iter2.hasNext());

        if (c1 instanceof Values.PList) {
            return ((CTo) RT.listColl(((ArrayList) r)));
        } else {
            return r;
        }
    }

    public static<
            From1,
            From2,
            From3,
            To,
            CFrom1 extends Collection<From1>,
            CFrom2 extends Collection<From2>,
            CFrom3 extends Collection<From3>,
            CTo extends Collection<To>>
    CTo map(CFrom1 c1,
            CFrom2 c2,
            CFrom3 c3,
            TriFunction<From1, From2, From3, To> mapper,
            Supplier<CTo> newer
    ) {
        CTo r;
        if (c1 instanceof Values.PList) {
            r = ((CTo) new ArrayList<>(c1.size()));
        } else {
            r = newer.get();
        }


        Iterator<From1> iter1 = c1.iterator();
        Iterator<From2> iter2 = c2.iterator();
        Iterator<From3> iter3 = c3.iterator();
        while (iter1.hasNext()) {
            expect(iter2.hasNext());
            expect(iter3.hasNext());
            From1 from1 = iter1.next();
            From2 from2 = iter2.next();
            From3 from3 = iter3.next();
            r.add(mapper.apply(from1, from2, from3));
        }
        expect(!iter2.hasNext());
        expect(!iter3.hasNext());

        if (c1 instanceof Values.PList) {
            return ((CTo) RT.listColl(((ArrayList) r)));
        } else {
            return r;
        }
    }

    //////////////////////////////////////////////////////////////////////////////

    public interface TriConsumer<T1, T2, T3> {
        void accept(T1 t1, T2 t2, T3 te);
    }

    public static <From, CFrom extends Collection<From>>
    void each(CFrom c1, Consumer<From> consumer) {
        for (From el : c1) {
            consumer.accept(el);
        }
    }

    public static<From1, From2, CFrom1 extends Collection<From1>, CFrom2 extends Collection<From2>>
    void each(CFrom1 c1,
            CFrom2 c2,
            BiConsumer<From1, From2> consumer
    ) {
        Iterator<From1> iter1 = c1.iterator();
        Iterator<From2> iter2 = c2.iterator();
        while (iter1.hasNext()) {
            expect(iter2.hasNext());
            From1 from1 = iter1.next();
            From2 from2 = iter2.next();
            consumer.accept(from1, from2);
        }
        expect(!iter2.hasNext());
    }

    public static<
            From1,
            From2,
            From3,
            CFrom1 extends Collection<From1>,
            CFrom2 extends Collection<From2>,
            CFrom3 extends Collection<From3>>
    void each(CFrom1 c1,
            CFrom2 c2,
            CFrom3 c3,
            TriConsumer<From1, From2, From3> consumer
    ) {
        Iterator<From1> iter1 = c1.iterator();
        Iterator<From2> iter2 = c2.iterator();
        Iterator<From3> iter3 = c3.iterator();
        while (iter1.hasNext()) {
            expect(iter2.hasNext());
            expect(iter3.hasNext());
            From1 from1 = iter1.next();
            From2 from2 = iter2.next();
            From3 from3 = iter3.next();
            consumer.accept(from1, from2, from3);
        }
        expect(!iter2.hasNext());
        expect(!iter3.hasNext());
    }
}
