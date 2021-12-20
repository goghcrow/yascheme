package xiao.lang2.expander2;

import xiao.lang2.Names;
import xiao.lang2.Procedures;

import java.util.HashMap;
import java.util.Map;

import static xiao.lang2.Misc.Nullable;
import static xiao.lang2.Procedures.cons;
import static xiao.lang2.Procedures.map;
import static xiao.lang2.Values.*;

// ::pico::
// pico, xiao.lang.expander0.Syntax
// use syntax objects only for identifiers
// type SyntaxObject = class Syntax<Name> | Tuple<Syntax>
// ::mini::
// uses syntax objects for list forms as well as identifiers
/**
 * @author chuxiaofeng
 */
public class Syntax {
    // datum and nested syntax objects
    public final Object e;
    // scopes that apply at all phases
    final ScopeSet scopes;
    final Map<String, Object> props; // properties

    private Syntax(Object e, ScopeSet scopes, Map<String, Object> props) {
        this.e = e;
        this.scopes = scopes;
        this.props = props;
    }

    public Object property(String key) {
        return props.get(key);
    }

    public Syntax property(String key, Object val) {
        HashMap<String, Object> copy = new HashMap<>(props);
        copy.put(key, val);
        return Syntax.of(e, scopes, copy);
    }

    public Object unbox() {
        if (e instanceof Syntax) {
            return ((Syntax) e).unbox();
        } else {
            return e;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return e.equals(((Syntax) o).e) && scopes.equals(((Syntax) o).scopes);
    }

    @Override
    public int hashCode() {
        int result = e.hashCode();
        result = 31 * result + scopes.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "#<syntax:" + toDatum(this) + ">";
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    static Map<String, Object> emptyProps() {
        return new HashMap<>();
    }

    public static Syntax empty() {
        return of(null, ScopeSet.empty(), emptyProps());
    }

    // FOR TEST ONLY
    public static Syntax of(Object e, Scope... scopes) {
        return new Syntax(e, ScopeSet.of(scopes), emptyProps());
    }

    public static Syntax of(Object e, ScopeSet scopes, Map<String, Object> props) {
        return new Syntax(e, scopes, props);
    }

    public static boolean isIdentifier(Object s) {
        return s instanceof Syntax && ((Syntax) s).e instanceof Symbol;
    }

    public static boolean isDot(Object s) {
        return isIdentifier(s) && ((Symbol) ((Syntax) s).e).name.equals(Names.DOT);
    }

    public static boolean isPair(Object s) {
        return s instanceof Syntax && Procedures.isPair(((Syntax) s).e);
    }

    public static boolean isList(Object s) {
        return s instanceof Syntax && Procedures.isList(((Syntax) s).e);
    }

    // 所有节点都 wrap 成 syntax
    // datum->Syntax : coerces to syntax with no scopes
    // normalized `datum->syntax` to accept an existing syntax object that
    // supplies the scope set for new syntax

    public static Syntax fromDatum(@Nullable Syntax stxC, Object v) {
        return fromDatum(stxC, v, null);
    }

    public static Syntax fromDatum(@Nullable Syntax stxC, Object v, @Nullable Syntax stxP) {
        if (v instanceof Syntax) {
            return ((Syntax) v);
        } else if (v instanceof PList) { // fast-route
            PList map = map(el -> fromDatum(stxC, el, stxC), ((PList) v));
            return wrap(stxC, map, stxP);
        } else if (v instanceof Pair) {
            Object cons = cons(
                    fromDatum(stxC, ((Pair) v).car, stxP),
                    fromDatum(stxC, ((Pair) v).cdr, stxP)
            );
            return wrap(stxC, cons, stxP);
        } else {
            // 字面量等直接包一层 syntax
            return wrap(stxC, v, stxP);
        }
    }

    static Syntax wrap(Syntax stxC, Object e, Syntax stxP) {
        Map<String, Object> props = stxP == null ? emptyProps() : stxP.props;
        if (stxC == null) {
            return Syntax.of(e, ScopeSet.empty(), props);
        } else {
            return Syntax.of(e, stxC.scopes, props);
        }
    }

    public static Object toDatum(Object s) {
        if (s instanceof Syntax) {
            return toDatum(((Syntax) s));
        } else {
            return s;
        }
    }

    // Syntax->datum : discard scopes produces a plain S-exp
    public static Object toDatum(Syntax s) {
        return toDatum1(s.e);
    }

    static Object toDatum1(Object se) {
        if (se instanceof Syntax) {
            return toDatum1(((Syntax) se).e);
        } else if (se instanceof PList) { // fast-route
            return map(Syntax::toDatum1, ((PList) se));
        } else if (se instanceof Pair) {
            return cons(
                    toDatum1(((Pair) se).car),
                    toDatum1(((Pair) se).cdr)
            );
        } else {
            return se;
        }
    }
}
