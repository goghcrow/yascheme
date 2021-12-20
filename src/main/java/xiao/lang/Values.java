package xiao.lang;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Procedures.*;

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
 * byte|short|int|long|float|double|bigint|bigdec
 *  : java.lang.Byte|Short|Integer|Long|Float|Double|BigInteger|BigDecimal
 */
public interface Values {

    interface Callable {

        void call(Object[] args, Env E, Ctx K);

        static Callable nameOf(String name, Callable callable) {
            if (callable instanceof Procedure) {
                return Procedure.nameOf(name, ((Procedure) callable));
            } else if (callable instanceof Syntax) {
                return Syntax.nameOf(name, ((Syntax) callable));
            } else {
                throw new InterpError("invalid callable: " + callable);
            }
        }
    }

    /**
     * Procedure / Closure / Continuation / JavaMethod
     */
    interface Procedure extends Callable {
        // void call(Object[] args, Env E, Ctx K);

        static Callable nameOf(String name, Procedure procedure) {
            return new NamedProcedure(name, procedure);
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

    interface Syntax extends Callable {
        // void call(Object[] forms, Env E, Ctx K);

        static Callable nameOf(String name, Syntax syntax) {
            return new NamedSyntax(name, syntax);
        }

        class NamedSyntax implements Syntax {
            final String name;
            final Syntax syntax;

            NamedSyntax(String name, Syntax syntax) {
                this.name = name;
                this.syntax = syntax;
            }

            @Override
            public void call(Object[] forms, Env E, Ctx K) {
                syntax.call(forms, E, K);
            }

            @Override
            public String toString() {
                return "#<syntax:" + name + ">";
            }
        }
    }


    final class Symbol {
        // 简化使用, 查表直接引用相等
        private final static Map<String, Symbol> cache = new HashMap<>();
        private final static AtomicInteger cnt_ = new AtomicInteger();

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
            return Symbol.of(prefix + "ϟ" + Symbol.cnt_.incrementAndGet(), false);
        }

        @Override
        public String toString() {
            // return "'" + name;
            return name;
        }
    }

    final class Keyword {
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

    final class Pair {
        public final Object car;
        public final Object cdr;

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
            return Objects.equals(car, pair.car) && Objects.equals(cdr, pair.cdr);

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
                return "(" + car + ")";
            } else {
                if (cdr instanceof Pair) {
                    String s = cdr.toString();
                    // String rmPar = s.substring(2, s.length() - 1);
                    // return "'(" + car + " " + rmPar + ")";
                    String rmPar = s.substring(1, s.length() - 1);
                    return "(" + car + " " + rmPar + ")";
                } else {
                    // return "'(" + car + " . " + cdr + ")";
                    return "(" + car + " . " + cdr + ")";
                }
            }
        }
    }

    class PList extends ArrayList<Object> {
        public PList() { }

        PList(int cap) {
            super(cap);
        }

        PList(Collection<Object> c) {
            super(c);
        }

        public PList unmodifiable() {
            return new UnmodifiablePList(this);
        }

        @Override
        public PList subList(int fromIndex, int toIndex) {
            return new PList(super.subList(fromIndex, toIndex));
        }

        @Override
        public boolean equals(Object o) {
            return super.equals(o);
        }

        @Override
        public String toString() {
            if (this.size() == 2) {
                if (get(0).equals(sym(Names.QUOTE))) {
                    return "'" + toString(get(1));
                }
            }
            StringJoiner sj = new StringJoiner(" ", "(", ")");
            // StringJoiner sj = new StringJoiner(" ", "'(", ")");
            for (Object o : this) {
                sj.add(toString(o));
            }
            return sj.toString();
        }

        static String toString(Object o) {
            if (o instanceof String) {
                return '"' + o.toString() + '"';
            } else {
                return Objects.toString(o);
            }
        }
    }

    // for 卫生宏使用
    final class SyntacticClosure {
        final Env E;
        final Object C;

        SyntacticClosure(Env E, Object C) {
            this.E = E;
            this.C = C;
        }

        @Override
        public String toString() {
            return "👻" + C;
        }
    }

    class UnmodifiablePList extends PList {
        final static UnmodifiablePList NULL = new UnmodifiablePList(0);
        public UnmodifiablePList() { }
        UnmodifiablePList(int cap) { super(cap); }
        UnmodifiablePList(Collection<Object> c) { super(c); }
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
        public PList subList(int fromIndex, int toIndex) {
            return new UnmodifiablePList(super.subList(fromIndex, toIndex));
        }
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


//    interface Tmp {
//        static void main(String[] args) {
//            // 采用这种方案
//            // 必须用 cons 构造 pair, 而不能 new Pair()
//            {
//                PList a = list(1, 2, 3);
//                Object b = cons(1, cons(2, cons(3, Null())));
//                Object c = cons(1, list(2, 3));
//                expect(a.equals(b), "assert fail");
//                expect(a.equals(c), "assert fail");
//                expect(b.equals(c), "assert fail");
//            }
//
//
//            // 如果要 new Pair() 则需要 normalize 函数和特殊处理的 isList
//            {
//                Pair a = new Pair(1, new Pair(2, new Pair(3, Null())));
//                Pair b = new Pair(1, cons(2, cons(3, Null())));
//                Pair c = new Pair(1, list(2, 3));
//
//                expect(isList(normalize(a)), "assert fail");
//                expect(isList(normalize(b)), "assert fail");
//                expect(isList(normalize(c)), "assert fail");
//
//                expect(normalize(a).equals(normalize(b)), "assert fail");
//                expect(normalize(a).equals(normalize(c)), "assert fail");
//                expect(normalize(b).equals(normalize(c)), "assert fail");
//            }
//        }
//
//
//        static PList normalize(Object o) {
//            if (o instanceof PList) {
//                return ((PList) o);
//            } else if (o instanceof Pair) {
//                PList lst = list();
//                lst.add(car(o));
//                lst.addAll(normalize(cdr(o)));
//                return lst;
//            } else {
//                throw new InterpError("contract violation");
//            }
//        }
//
//        static boolean isList(Object o) {
//            if (o instanceof PList) {
//                return true;
//            } else if (o instanceof Pair) {
//                return isList(((Pair) o).cdr);
//            } else {
//                return false;
//            }
//        }
//    }
}
