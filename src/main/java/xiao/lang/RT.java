package xiao.lang;

import java.util.*;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;
import java.util.stream.Collectors;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.sneakyThrows;
import static xiao.lang.Values.*;

/**
 * @author chuxiaofeng
 */
public interface RT {

    ToIntFunction<Number> toIntFunction = Number::intValue;
    ToLongFunction<Number> toLongFunction = Number::longValue;
    ToDoubleFunction<Number> toDoubleFunction = Number::doubleValue;

    // ------------------------------------------------------------

    static void throw1(Throwable t) {
        sneakyThrows(t);
    }

    static boolean eq(Object a, Object b) {
        return a == b;
    }

    static boolean equal(Object a, Object b) {
        return Objects.deepEquals(a, b);
    }

    // ------------------------------------------------------------

    static Symbol gensym() {
        return gensym("");
    }

    static Symbol gensym(String prefix) {
        return Symbol.gen(prefix);
    }

    static Symbol gensym(Symbol prefix) {
        return gensym(prefix.name);
    }

    // ------------------------------------------------------------

    static Object Void() {
        return Void.TYPE;
    }

    static PList Null() {
        return PList.NULL;
    }

    // ------------------------------------------------------------

    static boolean isKeyword(Object o) {
        return o instanceof Keyword;
    }

    static boolean isSymbol(Object o) {
        return o instanceof Symbol;
    }

    static boolean isPair(Object o) {
        return o instanceof Pair || (o instanceof PList && !isNull(o));
    }

    static boolean isList(Object o) {
        return o instanceof PList;
    }

    static boolean isNull(Object o) {
        return Null().equals(o);
    }

    static boolean isVector(Object o) {
        if (o == null) {
            return false;
        } else {
            return o.getClass().isArray();
        }
    }

    // ------------------------------------------------------------

    static Object car(Object o) {
        expect(isPair(o), "contract violation: car 期望 pair, 实际是 " + o);
        if (o instanceof Pair) {
            return ((Pair) o).car;
        } else {
            expect(!isNull(o), "不能 car null");
            return ((PList) o).get(0);
        }
    }

    static Object cdr(Object o) {
        expect(isPair(o), "类型错误: (car PAIR)");
        if (o instanceof Pair) {
            return ((Pair) o).cdr;
        } else {
            expect(!isNull(o), "不能 cdr null");
            return listColl(((PList) o).subList(1, ((PList) o).size()));
        }
    }

    // ------------------------------------------------------------

    static Keyword keyword(String name) {
        return Keyword.of(name);
    }

    static Symbol sym(String name) {
        return Symbol.of(name, true);
    }

    static PList cons(Object car, PList cdr) {
        return list(car, splice(cdr));
    }

    static Object cons(Object car, Object cdr) {
        if (cdr instanceof PList) {
            return cons(car, ((PList) cdr));
        } else {
            return new Pair(car, cdr);
        }
    }

    // ------------------------------------------------------------

    static Object[] vector(Object ...el) {
        return el;
    }

    static Object[] vectorColl(Collection<Object> c) {
        return c.toArray();
    }

    // ------------------------------------------------------------

    static PList listColl(Collection<Object> c) {
        if (c.isEmpty()) {
            return Null();
        } else {
            return new PList(c);
        }
    }

    static PList list(Object... args) {
        if (args.length == 0) {
            return Null();
        } else {
            return new PList(quasilist(args));
        }
    }

//    static PList list(byte... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            List<Object> lst = new ArrayList<>();
//            for (int i = 0; i < args.length; i++) {
//                lst.add(i, args[i]);
//            }
//            return new PList(lst);
//        }
//    }
//    static PList list(short... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            List<Object> lst = new ArrayList<>();
//            for (int i = 0; i < args.length; i++) {
//                lst.add(i, args[i]);
//            }
//            return new PList(lst);
//        }
//    }
//    static PList list(int... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            return new PList(Arrays.stream(args).boxed().collect(Collectors.toList()));
//        }
//    }
//    static PList list(long... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            return new PList(Arrays.stream(args).boxed().collect(Collectors.toList()));
//        }
//    }
//    static PList list(float... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            List<Object> lst = new ArrayList<>();
//            for (int i = 0; i < args.length; i++) {
//                lst.add(i, args[i]);
//            }
//            return new PList(lst);
//        }
//    }
//    static PList list(double... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            return new PList(Arrays.stream(args).boxed().collect(Collectors.toList()));
//        }
//    }
//    static PList list(char... args) {
//        if (args.length == 0) {
//            return Null();
//        } else {
//            List<Object> lst = new ArrayList<>();
//            for (int i = 0; i < args.length; i++) {
//                lst.add(i, args[i]);
//            }
//            return new PList(lst);
//        }
//    }

    class Splicing {
        final Collection<?> c;
        Splicing(Collection<?> c) { this.c = c; }
    }

    static Splicing splice(Collection<?> c) {
        return new Splicing(c);
    }

    static Splicing splice(Object[] arr) {
        return new Splicing(Arrays.asList(arr));
    }

    static PList quasilist(Object... args) {
        List<Object> lst = new ArrayList<>();
        for (Object it : args) {
            if (it instanceof Splicing) {
                lst.addAll(((Splicing) it).c);
            } else {
                lst.add(it);
            }
        }
        return listColl(lst);
    }

    // ------------------------------------------------------------

//    interface TriFunction<T1, T2, T3, R> {
//        R apply(T1 t1, T2 t2, T3 t3);
//    }

    static PList map(Function<Object, Object> mapper, PList lst) {
        List<Object> r = new ArrayList<>(lst.size());
        for (Object o : lst) {
            r.add(mapper.apply(o));
        }
        return RT.listColl(r);
    }

//    static PList map(BiFunction<Object, Object, Object> mapper, PList l1, PList l2) {
//        int sz = l1.size();
//        expect(sz == l2.size());
//        List<Object> r = new ArrayList<>(sz);
//        for (int i = 0; i < sz; i++) {
//            r.add(mapper.apply(l1.get(i), l2.get(i)));
//        }
//        return RT.listColl(r);
//    }
//
//    static PList map(TriFunction<Object, Object, Object, Object> mapper, PList l1, PList l2, PList l3) {
//        int sz = l1.size();
//        expect(sz == l2.size() && sz == l3.size());
//        List<Object> r = new ArrayList<>(sz);
//        for (int i = 0; i < sz; i++) {
//            r.add(mapper.apply(l1.get(i), l2.get(i), l3.get(i)));
//        }
//        return RT.listColl(r);
//    }

    static PList map(Function<PList, Object> mapper, PList... lst) {
        expect(lst.length > 0);
        int sz = lst[0].size();
        for (PList l : lst) {
            expect(sz == l.size());
        }
        List<Object> r = new ArrayList<>(sz);
        for (int i = 0; i < sz; i++) {
            List<Object> args = new ArrayList<>(lst.length);
            for (PList l : lst) {
                args.add(l.get(i));
            }
            r.add(mapper.apply(RT.listColl(args)));
        }
        return RT.listColl(r);
    }

    // ------------------------------------------------------------

//    static PList append() {
//        return Null();
//    }

//    static PList append(PList a) {
//        return listColl(a);
//    }

    static PList append(PList a, PList b) {
        if (isNull(a)) return listColl(b);
        if (isNull(b)) return listColl(a);
        return list(splice(a), splice(b));
    }

//    static PList append(PList... lst) {
//        return list(Arrays.stream(lst).map(RT::splice).toArray());
//    }

    static Object append(Object ...args) {
        if (args.length == 0) {
            return Null();
        } else {
            Object last = args[args.length - 1];
            if (last instanceof PList) {
                Object[] els = new Object[args.length];
                for (int i = 0; i < args.length; i++) {
                    expect(args[i] instanceof PList, "contract violation");
                    els[i] = splice((PList) args[i]);
                }
                return list(els);
            } else {
                Object el = last;
                for (int i = args.length - 2; i >= 0; i--) {
                    expect(args[i] instanceof PList, "contract violation");
                    PList lst = (PList) args[i];
                    for (int j = lst.size() - 1; j >= 0; j--) {
                        el = cons(lst.get(j), el);
                    }
                }
                return el;
            }
        }
    }

    static void main(String[] args) {
        System.out.println(append(1));
        System.out.println(append(Null(), 1));
        System.out.println(append(Null(), Null(), 1));
        System.out.println(append(list(1), 1));
        System.out.println(append(list(1, 2), list(3, 4), 1));
    }
}
