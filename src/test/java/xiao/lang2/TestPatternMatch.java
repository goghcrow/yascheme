package xiao.lang2;

import xiao.lang2.expander2.Syntax;

import static xiao.lang2.Pattern.*;
import static xiao.lang2.Pattern.Matcher.makeEmptyVars;
import static xiao.lang2.Procedures.*;
import static xiao.lang2.TestMisc.*;
import static xiao.lang2.Values.PList;

public interface TestPatternMatch {

    static PList list(Object... args) {
        return Procedures.list(args);
    }

    // todo 检查所有的 pair?


    // todo pair pattern 测试
    // todo 测试一些 cons 结构的 syntax-list
    static PList match(String pattern, Object s) {
        return new Matcher(s, Parser.parse1(pattern), new String[0]).doMatch();
    }
    static void tmp() {
        PList r3 = match("(let ([id val-expr] ...) body ...+)", Reader.read("(let ([+ (lambda (x y)\n" +
                "           (if (string? x)\n" +
                "             (string-append x y)\n" +
                "             (+ x y)))]) ; use original +\n" +
                "  (list (+ 1 2)\n" +
                "    (+ \"see\" \"saw\")))"));

        {
            Syntax s = Syntax.fromDatum(null, Parser.parse1(
                    "( (( ((a1 a2 a3) a4) ((b1 b2 b3) b4) ) x5) (( ((c1 c2 c3) c4) ((d1 d2 d3) d4) ) y5) )"
            ));
            PList r = match("(((((a ...) b) ...) c) ...)", s);
            System.out.println(r);
        }


        {
            Object form = Parser.parse1("(1 (2 3))");
            Syntax s = Syntax.fromDatum(null, form);

            PList r1 = match("(id1 id1)", form);
            System.out.println(r1);

            PList r2 = match("id1", s);
            System.out.println(r2);
        }
    }


    static void test() {
        tmp();
        test_makeEmptyVars();
        test_toSyntaxListEx();
        test_toSyntaxList();
        test_matchSyntax();

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

    static void test_toSyntaxListEx() {
        try {
            Matcher m = new Matcher(Syntax.of(null), "()", new String[0]);
            // (cons 1 2)
            Syntax s = Syntax.of(cons(Syntax.of(1), Syntax.of(2)));
            m.toSyntaxList(s);
            throw new RuntimeException();
        } catch (InterpError ignored) {}
    }

    static void test_toSyntaxList() {
        Matcher m = new Matcher(Syntax.of(null), "()", new String[0]);
        PList s123 = list(Syntax.of(1), Syntax.of(2), Syntax.of(3));

        {
            // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
            Syntax s = Syntax.of(
                    cons(
                            Syntax.of(1),
                            Syntax.of(list(Syntax.of(2), Syntax.of(3)))
                    )
            );
            PList sl = m.toSyntaxList(s);
            expectEquals(s123, sl);
        }
        {
            // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
            Syntax s = Syntax.of(
                    cons(
                            Syntax.of(1),
                            cons(
                                    Syntax.of(2),
                                    cons(
                                            Syntax.of(3),
                                            Syntax.of(Null())
                                    )
                            )
                    )
            );
            PList sl = m.toSyntaxList(s);
            expectEquals(s123, sl);
        }
    }

    static void test_matchSyntax() {
        {
            PList expr = list(sym("+"), 1, 1);
            Syntax s = Syntax.fromDatum(null,
                    list(
                            sym("define"),
                            sym("a"),
                            expr
                    ));
            {
                Finder r = Pattern.of1(Parser.parse1("(define id expr)")).match(s);
                expectEquals(r.get("expr"), Syntax.fromDatum(null, expr));
            }
            {
                Finder r = Pattern.of2(Parser.parse1("(define id expr)")).match(s);
                expectEquals(r.get("expr"), Syntax.fromDatum(null, expr));
            }

        }


        {
            String pattern = "(let-values ([(id ...) rhs] ...) body)";
            Syntax s = Syntax.fromDatum(null, Parser.parse1(
                    "(let-values ([(a b) (values 1 2)]\n" +
                            "             [(c d) (values 3 4)])\n" +
                            "`(,a ,b, c, d))"
            ));

            Syntax a = ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(0)).e).get(0)).e).get(0));
            Syntax b = ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(0)).e).get(0)).e).get(1));
            Syntax c = ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(1)).e).get(0)).e).get(0));
            Syntax d = ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(1)).e).get(0)).e).get(1));

            Syntax rhs12 = (Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(0)).e).get(1);
            Syntax rhs34 = (Syntax) ((PList) ((Syntax) ((PList) ((Syntax) ((PList) s.e).get(1)).e).get(1)).e).get(1);
            Syntax body = (Syntax) ((PList) s.e).get(2);

            {
                Finder r = of1(Parser.parse1(pattern)).match(s);
                expectEquals(r.get("let-values"), ((PList) s.e).get(0));
                expectEquals(r.get("id"),
                        list(
                                list(a, b),
                                list(c, d)
                        )
                );
                expectEquals(r.get("rhs"),
                        list(
                                rhs12,
                                rhs34
                        )
                );
                expectEquals(r.get("body"), body);
            }

            {
                Finder r = of2(Parser.parse1(pattern)).match(s);
                expectEquals(r.get("let-values"), ((PList) s.e).get(0));
                expectEquals(r.get("id"),
                        list(
                                list(a, b),
                                list(c, d)
                        )
                );
                expectEquals(r.get("rhs"),
                        list(
                                rhs12,
                                rhs34
                        )
                );
                expectEquals(r.get("body"), body);
            }
        }
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
