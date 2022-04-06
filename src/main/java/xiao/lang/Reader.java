package xiao.lang;

import xiao.lang.expander.Expander;
import xiao.lang.expander.Syntax;

import java.util.ArrayList;
import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Names.*;
import static xiao.lang.Values.*;

/**
 * @author chuxiaofeng
 */
public interface Reader {

    static Object read(String s) {
        List<Object> forms = Parser.parse(s);
        expect(!forms.isEmpty(), "代码为空");
        return read(Syntaxes.beginOf(forms));
    }

    static Object read(Object form) {
        form = transformStaticFieldAccessor(form);
        form = transformDot(form);
        return form;
    }

    static Syntax readSyntax(String s) {
        Object form = read(s);
        Object syntax = Syntax.fromDatum(null, form);
        //noinspection UnnecessaryLocalVariable
        Syntax introSyntax = Expander.namespaceSyntaxIntroduce(syntax);
        return introSyntax;
    }

    static Object transformDot(Object form) {
        if (form instanceof PList) {
            PList lst = (PList) form;
            if (lst.size() >= 3) {
                Object last2 = lst.get(lst.size() - 2);
                if (last2 instanceof Symbol && ((Symbol) last2).name.equals(DOT)) {
                    Object cons = transformDot(lst.get(lst.size() - 1));
                    for (int i = lst.size() - 3; i >= 0; i--) {
                        cons = RT.cons(transformDot(lst.get(i)), cons);
                    }
                    return cons;
                }
            }
            return RT.map(Reader::transformDot, lst);
        } else if (form instanceof Pair) {
            Pair cons = (Pair) form;
            return RT.cons(
                    transformDot(RT.car(cons)),
                    transformDot(RT.cdr(cons))
            );
        } else {
            return form;
        }
    }

    // ClassName/staticField ==> (. ClassName staticField)
    static Object transformStaticFieldAccessor(Object form) {
        if (form instanceof PList) {
            PList lst = (PList) form;
            int sz = lst.size();
            if (sz == 0) {
                return form;
            } else {
                List<Object> lst1 = new ArrayList<>(1 + sz);
                // 跳过第一个 (ClassName/staticMethod args ...) ==> (. ClassName staticMethod args ...)
                Object car = lst.get(0);
                if (car instanceof PList) {
                    lst1.add(transformStaticFieldAccessor(car));
                } else {
                    lst1.add(car);
                }
                for (int i = 1; i < sz; i++) {
                    lst1.add(transformStaticFieldAccessor(lst.get(i)));
                }
                return RT.listColl(lst1);
            }
        }
        // else if (form instanceof Pair) {}
        else if (form instanceof Symbol) {
            String id = ((Symbol) form).name;
            if (isStaticFieldAccess(form)) {
                // ClassName/staticField ==> (. ClassName staticField)
                int idx = id.indexOf(SLASH);
                String klass = id.substring(0, idx);
                String field = id.substring(idx + 1);
                return RT.list(RT.sym(DOT), RT.sym(klass), RT.sym(field));
            } else {
                return form;
            }
        } else {
            return form;
        }
    }

    ////////////////////////////////////////////////////////////////////////////////

    static boolean isInstanceFieldAccess(Object form) {
        return form instanceof Symbol
                && ((Symbol) form).name.length() > 1
                && ((Symbol) form).name.startsWith(DOT + HYPHEN);
    }

    static boolean isInstanceMemberAccess(Object form) {
        return form instanceof Symbol
                && ((Symbol) form).name.length() > 1
                && ((Symbol) form).name.startsWith(DOT)
                && !((Symbol) form).name.equals(DOT_DOT);
    }

    static boolean isStaticMethodCall(Object form) {
        return isJavaClassName(form)
                && ((Symbol) form).name.contains(SLASH);
    }

    static boolean isNewInstance(Object form) {
        return isJavaClassName(form)
                && ((Symbol) form).name.endsWith(DOT);
    }

    static boolean isStaticFieldAccess(Object form) {
        return isJavaClassName(form)
                && ((Symbol) form).name.contains(SLASH);
    }

    static boolean isJavaClassName(Object form) {
        return form instanceof Symbol
                && !((Symbol) form).name.equals(DOT_DOT)
                && ((Symbol) form).name.contains(DOT)
                && ((Symbol) form).name.length() > 1;
    }
}
