package xiao.lang.expander;

import xiao.lang.RT;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static xiao.lang.Contract.expect;

/**
 * @author chuxiaofeng
 */
public class DupCheck {

    // hash-eq
    static Map<Object, List<Syntax>> makeCheckNoDuplicateTable() {
        return new HashMap<>();
    }

    // type v = Syntax | List<v>
    static void checkNoDuplicateIds(Object ids, Syntax s) {
        Map<Object, List<Syntax>> ht = makeCheckNoDuplicateTable();
        checkNoDuplicateIds1(ids, ht);
    }

    // type v = Syntax | List<v>
    static void checkNoDuplicateIds(Object ids, Syntax s, Map<Object, List<Syntax>> ht) {
        checkNoDuplicateIds1(ids, ht);
    }

    // type v = Syntax | List<v>
    private
    static void checkNoDuplicateIds1(Object v, Map<Object, List<Syntax>> ht) {
        if (Syntax.isIdentifier(v)) {
            Syntax s = (Syntax) v;
            List<Syntax> l = ht.computeIfAbsent(s.e, k -> new ArrayList<>());
            for (Syntax id : l) {
                expect(!Scope.isBoundIdentifierEquals(id, s), "duplicate binding: " + v);
            }
            l.add(s);
        } else if (v instanceof List) {
            for (Object el : ((List<?>) v)) {
                checkNoDuplicateIds1(el, ht);
            }
        } else if (RT.isPair(v)) {
            checkNoDuplicateIds1(RT.car(v), ht);
            checkNoDuplicateIds1(RT.cdr(v), ht);
        }
    }

}
