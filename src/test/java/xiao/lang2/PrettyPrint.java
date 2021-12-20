package xiao.lang2;

import xiao.lang2.expander2.Expander;

import static xiao.lang2.Misc.resource;
import static xiao.lang2.Procedures.isNull;
import static xiao.lang2.Procedures.sym;
import static xiao.lang2.Values.*;

/**
 * @author chuxiaofeng
 */
public class PrettyPrint {

    public static void main(String[] args) {
        // String s = "(cons 1 2)";
        // String s = "(cons 1 (cons 2 3))";
        // String s = "(cons 1 (cons 2 (cons 3 4)))";
//        String s = "'(1 2 3)";
//
//        Interp interp = new Interp();
//        Env env = interp.newScope();
//        interp.interp(Reader.read(s), env, v -> {
//            System.out.println(pp(v));
//        });


        Object form = Reader.read(resource("/xx.ss"));
        System.out.println(pp(Expander.compileWithSyntax(form).sexpr));
    }

    public static String pp(Object o) {
        return new PrettyPrint().print(o).out.toString();
    }




    StringBuilder out = new StringBuilder();
    int indent = 0;
    boolean ignoreIndent = false;

    void indent() {
        if (ignoreIndent) {
            ignoreIndent = false;
        } else {
            String repeated = new String(new char[indent]).replace("\0", "  ");
            out.append(repeated);
        }
    }

    void break1() {
        out.append("\r\n");
    }

    void append(Object o) {
        indent();
        out.append(o);
    }

    PrettyPrint print(Object o) {
        return print(o, false);
    }

    PrettyPrint print(Object o, boolean ignoreParen) {
        if (o instanceof PList) {
            PList lst = (PList) o;
            if (lst.isEmpty()) {
                append("()");
                return this;
            }

            Object fst = lst.get(0);

            if (lst.size() == 2) {
                if (fst.equals(sym(Names.QUOTE))) {
                    append("'");
                    ignoreIndent = true;
                    print(lst.get(1));
                    return this;
                }
            }

            append("(");
            ignoreIndent = true;
            boolean isFirst = true;
            for (Object it : lst) {
                if (isFirst) {
                    isFirst = false;
                    print(it);
                    indent++;
                } else {
                    // append(" ");
                    break1();
                    print(it);
                }
            }
            ignoreIndent = true;
            append(")");
            indent--;
        } else if (o instanceof Pair) {
            Pair p = (Pair) o;
            if (isNull(p.cdr)) {
                append("(");
                print(p.car);
                append(")");
            } else {
                if (p.cdr instanceof Pair) {
                    if (!ignoreParen) {
                        append("(");
                    }
                    print(p.car);
                    append(" ");
                    print(p.cdr, true);
                    if (!ignoreParen) {
                        append(")");
                    }
                } else {
                    if (!ignoreParen) {
                        append("(");
                    }
                    print(p.car);
                    append(" . ");
                    print(p.cdr);
                    if (!ignoreParen) {
                        append(")");
                    }
                }
            }
        } else if (o instanceof String) {
            append("\"" + o + "\"");
        } else {
            if (o instanceof Procedure) {
                //#<procedure:cons>
                String s = o.toString();
                if (s.startsWith("#<procedure:")) {
                    append(s.substring("#<procedure:".length(), s.length() - 1));
                } else {
                    append(o);
                }
            } else {
                append(o);
            }
        }
        return this;
    }
}
