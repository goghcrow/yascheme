package xiao.lang;

import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Names.*;
import static xiao.lang.Procedures.*;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Symbol;

/**
 * @author chuxiaofeng
 */
public class Reader {

    public static Object read(String s) {
        List<Object> forms = Parser.parse(s);
        expect(!forms.isEmpty(), "代码为空");
        return read(Syntaxes.beginOf(forms));
    }

    // 递归无脑展开
    public static Object read(Object form) {
        // todo 用 expander 来实现
        return MemberAccessorExpansion.expandStaticFieldAccessor(form);
    }

    public static class MemberAccessorExpansion {

        // 递归展开
        // ClassName/staticField ==> (. ClassName staticField)
        public static Object expandStaticFieldAccessor(Object form) {
            if (form instanceof PList) {
                // 跳过第一个 (ClassName/staticMethod args ...) ==> (. ClassName staticMethod args ...)
                PList lst = (PList) form;
                if (lst.size() <= 1) {
                    return form;
                } else {
                    PList lst1 = listCap(1 + lst.size());
                    lst1.add(lst.get(0));
                    for (int i = 1; i < lst.size(); i++) {
                        lst1.add(expandStaticFieldAccessor(lst.get(i)));
                    }
                    return lst1.unmodifiable();
                }
            } else if (form instanceof Symbol) {
                String id = ((Symbol) form).name;
                if (isStaticFieldAccess(form)) {
                    // ClassName/staticField ==> (. ClassName staticField)
                    int idx = id.indexOf(SLASH);
                    String klass = id.substring(0, idx);
                    String field = id.substring(idx + 1);
                    return list(sym(DOT), sym(klass), sym(field));
                } else {
                    return form;
                }
            } else {
                return form;
            }
        }

        /*
        (.instanceMember instance-expr args ...) ==> (. instance-expr instanceMember args ...)
        (.instanceMember ClassName args ...) ==> (. ClassName instanceMember args ...)
        (.-instanceField instance-expr) ==> (. instance-expr -instanceField)
        (ClassName/staticMethod args ...) ==> (. ClassName staticMethod args ...)
        ClassName/staticField ==> (. ClassName staticField)

        (ClassName. args*) ==> (new ClassName args*)
         */
        // 只展开一层
        public static Object expandMemberAccessor(Object form) {
            if (form instanceof PList) {
                return expandMemberAccessor(((PList) form));
            } else {
                return form;
            }
        }

        static List<Object> expandMemberAccessor1(List<Object> forms) {
//            List<Object> lst = new ArrayList<>();
//            for (Object form : forms) {
//                lst.add(expandMemberAccessor(form));
//            }
//            return lst;
            return forms;
        }

        static Object expandMemberAccessor(PList els) {
            if (els.isEmpty()) {
                return els;
            }
            Object fst = els.get(0);
            if (!(fst instanceof Symbol)) {
                // return list(expandMemberAccessor(els));
                return els;
            }
            String id = ((Symbol) fst).name;
            if (id.equals(SYNTAX_RULES)) {
                // 不处理 syntax_rules, 会影响 syntax 匹配, 等宏展开之后再替换
                return els;
            }

            if (isInstanceMemberAccess(fst)) {
                if (isInstanceFieldAccess(fst)) {
                    // (.-instanceField instance-expr) ==> (. instance-expr -instanceField)
                    expect(els.size() == 2, "(.-instanceField instance-expr)");
                    // return list(symbol(DOT), expandMemberAccessor(els.get(1)), nameOf(id.substring(1)));
                    return list(sym(DOT), els.get(1), sym(id.substring(1)));
                } else {
                    // (.instanceMember instance-expr args ...) ==> (. instance-expr (instanceMember args ...))
                    // (.instanceMember ClassName args ...) ==> (. ClassName (instanceMember args ...))
                    expect(els.size() >= 2,
                            "(.instanceMember instance-expr args ...) or (.instanceMember ClassName args ...)");
                    List<Object> args = expandMemberAccessor1(els.subList(2, els.size()));
                    return list(
                            sym(DOT),
                            els.get(1),
                            list(
                                    sym(id.substring(1)),
                                    splice(args)
                            )
                    );
                }
            } else if (isStaticMethodCall(fst)) {
                // (ClassName/staticMethod args ...) ==> (. ClassName (staticMethod args ...))
                int idx = id.indexOf(SLASH);
                String klass = id.substring(0, idx);
                String method = id.substring(idx + 1);
                List<Object> args = expandMemberAccessor1(els.subList(1, els.size()));
                return list(
                        sym(DOT),
                        sym(klass),
                        list(
                                sym(method),
                                splice(args)
                        )
                );
            } else if (isNewInstance(fst)) {
                // (ClassName. args ...) ==> (new ClassName args ...)
                String klass = id.substring(0, id.length() - 1);
                List<Object> args = expandMemberAccessor1(els.subList(1, els.size()));
                return list(
                        sym(NEW),
                        sym(klass),
                        splice(args)
                );
            } else {
                // return list(expandMemberAccessor(els));
                return els;
            }
        }

        public static boolean isInstanceFieldAccess(Object form) {
            return form instanceof Symbol
                    && ((Symbol) form).name.length() > 1
                    && ((Symbol) form).name.startsWith(DOT + HYPHEN);
        }

        public static boolean isInstanceMemberAccess(Object form) {
            return form instanceof Symbol
                    && ((Symbol) form).name.length() > 1
                    && ((Symbol) form).name.startsWith(DOT)
                    && !((Symbol) form).name.equals(DOT_DOT);
        }

        public static boolean isStaticMethodCall(Object form) {
            return isJavaClassName(form)
                    && ((Symbol) form).name.contains(SLASH);
        }

        public static boolean isNewInstance(Object form) {
            return isJavaClassName(form)
                    && ((Symbol) form).name.endsWith(DOT);
        }

        public static boolean isStaticFieldAccess(Object form) {
            return isJavaClassName(form)
                    && ((Symbol) form).name.contains(SLASH);
        }

        public static boolean isJavaClassName(Object form) {
            return form instanceof Symbol
                    && !((Symbol) form).name.equals(DOT_DOT)
                    && ((Symbol) form).name.contains(DOT)
                    && ((Symbol) form).name.length() > 1;
        }
    }
}
