package xiao.lang2;

import static xiao.lang2.Pattern.*;
import static xiao.lang2.Pattern.Matcher.makeEmptyVars;
import static xiao.lang2.Procedures.Null;
import static xiao.lang2.Procedures.sym;
import static xiao.lang2.TestMisc.*;
import static xiao.lang2.Values.PList;

public interface TestPatternMatch {

    static PList list(Object... args) {
        return Procedures.list(args);
    }

    static PList match(String pattern, Object s) {
        return new Matcher(s, Parser.parse1(pattern), new String[0]).doMatch();
    }

    static void test() {
        test_makeEmptyVars();

        test_id();
        test_cases();
    }


    static void test_makeEmptyVars() {
        expectEquals(
                makeEmptyVars(Parser.parse1("(((a ...) ...) ...)")),
                list(list(sym("a"), Null()))
        );
        expectEquals(
                makeEmptyVars(Parser.parse1("(a (b) c)")),
                list(
                        list(sym("a"), Null()),
                        list(sym("b"), Null()),
                        list(sym("c"), Null())
                )
        );
        expectEquals(
                makeEmptyVars(Parser.parse1("(a (b ...) ...)")),
                list(
                        list(sym("a"), Null()),
                        list(sym("b"), Null())
                )
        );
    }

    static void test_id() {
        expectTrue(Pattern.ofStr("id").tryMatch(Parser.parse1("(1 2 3)")) == null);
        expectTrue(Pattern.ofStr("id:lst").tryMatch(Parser.parse1("(1 2 3)")) == null);
        expectTrue(Pattern.ofStr("not_id").tryMatch(Parser.parse1("(1 2 3)")) != null);
    }


    static void test_cases() {
        assert1("1", new String[0], "1", true);
        assert1("1", new String[0], "42", false);
        assert1("3.14", new String[0], "3.14", true);
        assert1("3.14", new String[0], "42", false);
        assert1("3.14f", new String[0], "3.14", false);
        assert1("3.14", new String[0], "42", false);
        assert1("#t", new String[0], "#t", true);
        assert1("#t", new String[0], "#f", false);
        assert1("\"hello\"", new String[0], "\"hello\"", true);
        assert1("\"hello\"", new String[0], "\"world\"", false);

        // 注意: 去过将 symbol 当字面量匹配, 需要把 quote 加入 formal
        assert1("'s", new String[] { "quote" }, "'s", true);
        assert1("'s", new String[] { "quote" }, "'a", false);
        assert1("(x (y 'z))", new String[] { "quote" }, "(1 (2 'z))", true, "x", 1, "y", 2);
        assert1("(x (y 'x))", new String[] { "quote" }, "(1 (2 'z))", false);


        assert1("(1 (3.14 (#t #f) (\"hello\" 's)) a)", new String[] { "quote" },
                "(1 (3.14 (#t #f) (\"hello\" 's)) 42)", true, "a", 42);
        assert1("(1 (#t #f) a)", new String[0], "(1 (#f #t) 42)", false);

        assert1("(define name val)", new String[] { "define" }, "(define a)", false);

        assert1("(define name val)", new String[] { "define" }, "(define a (+ 1 2))", true,
                "name", sym("a"),
                "val", list(sym("+"), 1, 2));

        assert1("a", new String[0], "1", true, "a", 1);
        assert1("(a)", new String[0], "(1)", true, "a", 1);
        assert1("(a b)", new String[0], "(1 2)", true,
                "a", 1, "b", 2);
        assert1("(a b)", new String[0], "(1 (2 3))", true,
                "a", 1, "b", list(2, 3));

        assert1("([var init step ...] ...)", new String[0], "((a 0) (b 0 1) (c 0 1 2))", true,
                "var", lists(sym("a"), sym("b"), sym("c")),
                "init", lists(0, 0, 0),
                "step", lists(lists(), lists(1), lists(1, 2)));

        assert1("(a ...)", new String[0], "()", true, "a", lists());
        assert1("(a ...)", new String[0], "(1)", true, "a", lists(1));
        assert1("(a ...+)", new String[0], "()", false);
        assert1("(a ...+)", new String[0], "(1)", true, "a", lists(1));
    }


    static void assert1(String ptn, String[] formals, String form, boolean matched, Object ...pairs) {
        Object pattern = Parser.parse1(ptn);
        Finder r1 = of1(pattern, formals).tryMatch(Parser.parse1(form));
        cmp(r1, ptn, form, matched, pairs);

        Finder r2 = of2(pattern, formals).tryMatch(Parser.parse1(form));
        cmp(r2, ptn, form, matched, pairs);
    }

    static void cmp(Finder r, String ptn, String form, boolean matched, Object ...pairs) {
        // System.out.println(r);
        String msg = ptn + " ~> " + form;
        if (matched) {
            expectTrue(r != null, msg);
            for (int i = 0; i < pairs.length; i += 2) {
                expectEquals(r.get(((String) pairs[i])), pairs[i + 1], msg);
            }
        } else {
            expectTrue(r == null, msg);
        }
    }

}
