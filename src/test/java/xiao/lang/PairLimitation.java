package xiao.lang;

import java.util.ArrayList;
import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Pair;

public interface PairLimitation {

    static void main(String[] args) {
        // 采用这种方案
        // 必须用 cons 构造 pair, 而不能 new Pair()
        {
            PList a = RT.list(1, 2, 3);
            Object b = RT.cons(1, RT.cons(2, RT.cons(3, RT.Null())));
            Object c = RT.cons(1, RT.list(2, 3));
            expect(a.equals(b), "assert fail");
            expect(a.equals(c), "assert fail");
            expect(b.equals(c), "assert fail");
        }


        // 如果要 new Pair() 则需要 normalize 函数和特殊处理的 isList
        {
            Pair a = new Pair(1, new Pair(2, new Pair(3, RT.Null())));
            Pair b = new Pair(1, RT.cons(2, RT.cons(3, RT.Null())));
            Pair c = new Pair(1, RT.list(2, 3));

            expect(isList(normalize(a)), "assert fail");
            expect(isList(normalize(b)), "assert fail");
            expect(isList(normalize(c)), "assert fail");

            expect(normalize(a).equals(normalize(b)), "assert fail");
            expect(normalize(a).equals(normalize(c)), "assert fail");
            expect(normalize(b).equals(normalize(c)), "assert fail");
        }
    }


    static PList normalize(Object o) {
        if (o instanceof PList) {
            return ((PList) o);
        } else if (o instanceof Pair) {
            List<Object> lst = new ArrayList<>();
            lst.add(RT.car(o));
            lst.addAll(normalize(RT.cdr(o)));
            return RT.listColl(lst);
        } else {
            throw new InterpError("contract violation");
        }
    }

    static boolean isList(Object o) {
        if (o instanceof PList) {
            return true;
        } else if (o instanceof Pair) {
            return isList(((Pair) o).cdr);
        } else {
            return false;
        }
    }
}
