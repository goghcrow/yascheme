package xiao.lang.expander;

import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.StringJoiner;
import java.util.function.Consumer;

/**
 * 顺序无关, equals, isSubset 都不需要检查 order
 * @author chuxiaofeng
 */
public class ScopeSet implements Serializable {

    public static ScopeSet empty() {
        return ScopeSet.of();
    }

    public static ScopeSet of(Scope... scopes) {
        LinkedHashSet<Scope> set = new LinkedHashSet<>();
        Collections.addAll(set, scopes);
        return new ScopeSet(set);
    }

    // 这里 HashSet 就可以, 顺序无关, 用 Linked 是为了调试方便
    final LinkedHashSet<Scope> set;

    private ScopeSet(LinkedHashSet<Scope> set) {
        this.set = set;
    }

    ScopeSet adjust(Consumer<LinkedHashSet<Scope>> map) {
        LinkedHashSet<Scope> set = new LinkedHashSet<>(this.set);
        map.accept(set);
        return new ScopeSet(set);
    }

    ScopeSet add(Scope sc) {
        return adjust(set -> set.add(sc));
    }

    ScopeSet add(ScopeSet scs) {
        return adjust(set -> set.addAll(scs.set));
    }

    ScopeSet remove(Scope sc) {
        return adjust(set -> set.remove(sc));
    }

    ScopeSet remove(ScopeSet scs) {
        return adjust(set -> set.removeAll(scs.set));
    }


    ScopeSet flip(Scope sc) {
        return adjust(set -> {
            if (set.contains(sc)) {
                set.remove(sc);
            } else {
                set.add(sc);
            }
        });
    }

    boolean isSubset(ScopeSet o) {
        return o.set.containsAll(set);
    }

    int count() {
        return set.size();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        // containsAll!!!
        return set.equals(((ScopeSet) o).set);
    }

    @Override
    public int hashCode() {
        return set.hashCode();
    }

    @Override
    public String toString() {
        StringJoiner sj = new StringJoiner(", ", "ScopeSet {", "}");
        for (Scope scope : set) {
            sj.add(scope.toString());
            // sj.add(String.valueOf(scope.id));
        }
        return sj.toString();
    }
}
