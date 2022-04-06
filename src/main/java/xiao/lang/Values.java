package xiao.lang;

import java.io.*;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.RT.isNull;
import static xiao.lang.RT.sym;

/**
 * @author chuxiaofeng
 *
 * 类型映射表 <br/>
 * void : java.lang.Void.Type <br/>
 * pair : xiao.lang.Values.Pair <br/>
 * null : xiao.lang.Values.PList.NULL <br/>
 * list : xiao.lang.Values.PList <br/>
 * string : java.lang.String <br/>
 * symbol : xiao.lang.Values.Symbol <br/>
 * #t   : java.lang.Boolean.TRUE <br/>
 * #f   : java.lang.Boolean.FALSE <br/>
 * char|byte|short|int|long|float|double|bigint|bigdec
 *  : java.lang.Charactor|Byte|Short|Integer|Long|Float|Double|BigInteger|BigDecimal
 * vector : array
 *
 *  fixme 目前 Pair & PList 数据结构无法实现 set-cdr!
 */
public interface Values {

    /**
     * Procedure / Closure / Continuation / JavaMethod
     */
    interface Procedure {

        void call(Object[] args, Env E, Ctx K);

        static Procedure nameOf(String name, Procedure procedure) {
            // #<procedure: 有冒号说明是有名字的, 是在 java 里头定义的
            // #<procedure> 是没有名字的, 在 ss 中定义的
            if (procedure.toString().startsWith("#<procedure:")) {
                return procedure;
            } else {
                return new NamedProcedure(name, procedure);
            }
        }

        class NamedProcedure implements Procedure {
            final String name;
            final Procedure proc;

            NamedProcedure(String name, Procedure proc) {
                this.name = name;
                this.proc = proc;
            }

            @Override
            public void call(Object[] args, Env E, Ctx K) {
                proc.call(args, E, K);
            }

            @Override
            public String toString() {
                return "#<procedure:" + name + ">";
            }
        }
    }

    interface Syntax {

        void call(Object[] forms, Env E, Ctx K);

    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    final class Symbol implements Serializable {
        // 简化使用, 查表直接引用相等
        private final static Map<String, Symbol> cache = new HashMap<>();
        // private final static AtomicInteger cnt_ = new AtomicInteger();
        private static int cnt_ = 0;

        public final String name;

        private Symbol(String name) {
            this.name = name;
        }

        static Symbol of(String name, boolean intern) {
            if (intern) {
                return Symbol.cache.computeIfAbsent(name, t -> new Symbol(name));
            } else {
                return new Symbol(name);
            }
        }
        static Symbol gen(String prefix) {
            // "__ϟ😓"
            return Symbol.of(prefix + "__ϟ" + ++cnt_, false);
        }



        // 注意
        // readResolve is called after readObject has returned
        // writeReplace is called before writeObject
        // private Object writeReplace() throws ObjectStreamException { }
        private Object readResolve() throws ObjectStreamException {
            // 反序列化替换下 intern 对象
            return of(name, true);
        }

        // 应该是不用区分 intern, 不需要 intern 是运行时的, 展开期没有
        private void writeObject(ObjectOutputStream oos) throws IOException {
            oos.defaultWriteObject();
            oos.writeObject(name);
        }

        private void readObject(ObjectInputStream ois) throws ClassNotFoundException, IOException {
            ois.defaultReadObject();
            String name = (String) ois.readObject();
            Reflect.instance(this).field("name").set(name);
        }
        @Override
        public String toString() {
            // return "'" + name;
            return name;
        }
    }

    final class Keyword implements Serializable {
        private final static Map<String, Keyword> cache = new HashMap<>();

        public final String name;

        private Keyword(String name) {
            expect(name.startsWith(Names.KEYWORD_PREFIX), "错误的 keyword: " + name);
            this.name = name;
        }

        static Keyword of(String name) {
            return Keyword.cache.computeIfAbsent(name, t -> new Keyword(name));
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return name.equals(((Keyword) o).name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }

        @Override
        public String toString() {
            return name;
        }
    }

    final class Pair implements Serializable {
        public final Object car;
        public final Object cdr;

        // 必须用 cons 构造 pair, 不能直接 new Pair()
        // private
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Pair pair = (Pair) o;
            return RT.equal(car, pair.car) && RT.equal(cdr, pair.cdr);

        }

        @Override
        public int hashCode() {
            int result = car != null ? car.hashCode() : 0;
            result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
            return result;
        }

        @Override
        public String toString() {
            if (isNull(cdr)) {
                // return "'(" + car + ")";
                return "(" + Values.toString(car) + ")";
            } else {
                if (cdr instanceof Pair) {
                    String s = cdr.toString();
                    // String rmPar = s.substring(2, s.length() - 1);
                    // return "'(" + car + " " + rmPar + ")";
                    String rmPar = s.substring(1, s.length() - 1);
                    return "(" + Values.toString(car) + " " + rmPar + ")";
                } else {
                    // return "'(" + car + " . " + cdr + ")";
                    return "(" + Values.toString(car) + " . " + Values.toString(cdr) + ")";
                }
            }
        }
    }

    class PList extends ArrayList<Object> {
        final static PList NULL = new PList(0);

        private PList(int cap) {
            super(cap);
        }

        PList(Collection<Object> c) {
            super(c);
        }

        @SuppressWarnings("unused")
        private Object set_(int idx, Object el) {
            return super.set(idx, el);
        }

        @Override
        public PList subList(int fromIndex, int toIndex) {
            return new PList(super.subList(fromIndex, toIndex));
        }

        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof PList)) {
                return false;
            }

            ListIterator<Object> e1 = listIterator();
            ListIterator<Object> e2 = ((PList) o).listIterator();
            while (e1.hasNext() && e2.hasNext()) {
                Object o1 = e1.next();
                Object o2 = e2.next();
                if (!(RT.equal(o1, o2))) {
                    return false;
                }
            }
            return !(e1.hasNext() || e2.hasNext());
        }

        @Override
        public String toString() {
            if (this.size() == 2) {
                if (get(0).equals(sym(Names.QUOTE))) {
                    return "'" + Values.toString(get(1));
                }
            }
            StringJoiner sj = new StringJoiner(" ", "(", ")");
            // StringJoiner sj = new StringJoiner(" ", "'(", ")");
            for (Object o : this) {
                sj.add(Values.toString(o));
            }
            return sj.toString();
        }

        public boolean add(Object e) { throw new UnsupportedOperationException(); }
        public boolean remove(Object o) { throw new UnsupportedOperationException(); }
        public boolean addAll(Collection<?> coll) { throw new UnsupportedOperationException(); }
        public boolean removeAll(Collection<?> coll) { throw new UnsupportedOperationException(); }
        public boolean retainAll(Collection<?> coll) { throw new UnsupportedOperationException(); }
        public void clear() { throw new UnsupportedOperationException(); }
        public boolean removeIf(Predicate<? super Object> filter) { throw new UnsupportedOperationException(); }
        public Object set(int index, Object element) { throw new UnsupportedOperationException(); }
        public void add(int index, Object element) { throw new UnsupportedOperationException(); }
        public Object remove(int index) { throw new UnsupportedOperationException(); }
        public boolean addAll(int index, Collection<?> c) { throw new UnsupportedOperationException(); }
        public void replaceAll(UnaryOperator<Object> operator) { throw new UnsupportedOperationException(); }
        public void sort(Comparator<? super Object> c) { throw new UnsupportedOperationException(); }
        public Iterator<Object> iterator() {
            Iterator<Object> i = super.iterator();
            return new Iterator<Object>() {
                public boolean hasNext() { return i.hasNext(); }
                public Object next()     { return i.next(); }
                public void remove()     { throw new UnsupportedOperationException(); }
                @Override
                public void forEachRemaining(Consumer<? super Object> action) {
                    i.forEachRemaining(action);
                }
            };
        }
        public ListIterator<Object> listIterator()   {return listIterator(0);}
        public ListIterator<Object> listIterator(final int index) {
            ListIterator<?> i = super.listIterator(index);
            return new ListIterator<Object>() {
                public boolean hasNext()     { return i.hasNext(); }
                public Object next()         { return i.next(); }
                public boolean hasPrevious() { return i.hasPrevious(); }
                public Object previous()     { return i.previous(); }
                public int nextIndex()       { return i.nextIndex(); }
                public int previousIndex()   { return i.previousIndex(); }
                public void remove() { throw new UnsupportedOperationException(); }
                public void set(Object e) { throw new UnsupportedOperationException(); }
                public void add(Object e) { throw new UnsupportedOperationException(); }
            };
        }
    }

    static String toString(Object o) {
        if (o instanceof String) {
            return '"' + o.toString() + '"';
        } else if (o == Boolean.TRUE) {
            return Names.TRUE;
        } else if (o == Boolean.FALSE) {
            return Names.FALSE;
        } else if (o == null) {
            return "NULL";
        } else if (o.getClass().isArray()) {
            return Interop.CallSite.method(Arrays.class, "toString")
                    .call(null, new Object[] { o }).toString();
        } else {
            return Objects.toString(o);
        }
    }

}

/*
    A pair combines exactly two values.
    The first value is accessed with the car procedure, and the second value is accessed with the cdr procedure.
    pair = car cdr
    list = null || (value list)
    A list is recursively defined: it is either the constant null, or it is a pair whose second value is a list.

Fundamental List Functions
    L = (x1 ... xn)
    null? : All → Boolean
    cons? : All → Boolean
    cons : All × List → Cons (cons x L) = (x x1 ... xn)
    first : Cons → All (first (cons x L)) = x
    rest : Cons → List (rest (cons x L)) = L

Building Lists
    > (list x1 ... xn) [macro – variable # of args] (cons x1 (cons x2 ... (cons xn nil)...))
    > (cons? ‘()) => #f
    > (null? ‘()) => #t
    > (cons 1 ‘()) => ‘(1)
    (cons? (cons 1 ‘())) => #t
    (cons 1 (cons 2 (cons 3 ‘()))) => ‘(1 2 3) (list 1 2 3) => ‘(1 2 3)
    (cons (cons 1 ‘()) ‘()) => ‘((1))

More About cons
    A cons cell contains two fields
        first [also called car]
        rest [also called cdr]
    For a list the rest field must be a list
    Generally both fields of a cons ∈ All
        (cons 1 2) = (1 . 2)
        Called a dotted pair
        first and rest require that the inputs are lists whereas car and cdr do not
*/
