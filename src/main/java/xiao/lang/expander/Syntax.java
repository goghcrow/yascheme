package xiao.lang.expander;

import xiao.lang.Pattern;
import xiao.lang.RT;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import static xiao.lang.Misc.Nullable;
import static xiao.lang.RT.*;
import static xiao.lang.Values.*;
import static xiao.lang.expander.Expansion.rebuild;
import static xiao.lang.expander.SyntaxMatcher.match;
import static xiao.lang.expander.SyntaxMatcher.tryMatch;

// ::pico::
// pico, xiao.lang.expander0.Syntax
// use syntax objects only for identifiers
// type SyntaxObject = class Syntax<Name> | Tuple<Syntax>
// ::mini::
// uses syntax objects for list forms as well as identifiers
/**
 * @author chuxiaofeng
 */
public class Syntax implements Serializable {
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
        return s instanceof Syntax && RT.isSymbol(((Syntax) s).e);
    }

    public static boolean isPair(Object s) {
        return s instanceof Syntax && RT.isPair(((Syntax) s).e);
    }

    public static boolean isList(Object s) {
        return s instanceof Syntax && RT.isList(((Syntax) s).e);
    }

    // ??????????????? wrap ??? syntax
    // datum->Syntax : coerces to syntax with no scopes
    // normalized `datum->syntax` to accept an existing syntax object that
    // supplies the scope set for new syntax

    // Converts the datum v to a syntax object. If v is already a syntax object,
    // then there is no conversion, and v is returned unmodified.
    // The contents of pairs are recursively converted.
    // For any kind of value conversion means wrapping the value with lexical information.
    public static Syntax fromDatum(@Nullable Syntax stxC, Object v) {
//        if (stxC == Core.coreStx) {
//            return fromDatum(stxC, v, Core.coreStx);
//        } else {
            return fromDatum(stxC, v, null);
//        }
    }

    // Converted objects in v are given the lexical context information of stxC and the prop information of stxP.
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
            // ??????????????????????????? syntax
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

    public static Object toList(Syntax s) /* : list? */ {
        Object d = toDatum(s);
        if (RT.isList(d)) {
            return d;
        } else {
            return false;
        }
    }
}
