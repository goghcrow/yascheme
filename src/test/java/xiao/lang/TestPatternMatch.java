package xiao.lang;

import xiao.lang.expander.Syntax;
import xiao.lang.pattern.SyntaxRule;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static xiao.lang.Pattern.Finder;
import static xiao.lang.Pattern.Matcher;
import static xiao.lang.Values.PList;

public interface TestPatternMatch {

    static void main(String[] args) {
        test_makeEmptyVars();
        test_toSyntaxListEx();
        test_toSyntaxList();
        test_matchSyntax();
        test_id();
        test_cases();
        test_nested();
        test_dup();
    }

    static Pattern of1(Object patternForm) {
        return of1(patternForm, new String[0]);
    }

    static Pattern of1(Object patternForm, String[] formals) {
        return s -> new SyntaxRule(patternForm, formals).match(s);
    }


    static Pattern of2(Object patternForm) {
        return of2(patternForm, new String[0]);
    }

    static Pattern of2(Object patternForm, String[] formals) {
        return s -> Matcher.wrapFinder(new Matcher(s, patternForm, formals).doMatch());
    }


    static PList match(String pattern, Object s) {
        return new Matcher(s, Parser.parse1(pattern), new String[0]).doMatch();
    }

    static void test_nested() {
            Syntax s = Syntax.fromDatum(null, Parser.parse1(
                    "( (( ((a1 a2 a3) a4) ((b1 b2 b3) b4) ) x5) (( ((c1 c2 c3) c4) ((d1 d2 d3) d4) ) y5) )"
            ));
            PList r = match("(((((a ...) b) ...) c) ...)", s);
            expectEquals(r.toString(),
            "(" +
                    "(a " +
                        "(" +
                            "(" +
                                "(#<syntax:a1> #<syntax:a2> #<syntax:a3>) " +
                                "(#<syntax:b1> #<syntax:b2> #<syntax:b3>)" +
                            ") " +
                            "(" +
                                "(#<syntax:c1> #<syntax:c2> #<syntax:c3>) " +
                                "(#<syntax:d1> #<syntax:d2> #<syntax:d3>)" +
                            ")" +
                        ")" +
                    ") " +
                    "(b " +
                        "(" +
                            "(#<syntax:a4> #<syntax:b4>) " +
                            "(#<syntax:c4> #<syntax:d4>)" +
                        ")" +
                    ") " +
                    "(c (#<syntax:x5> #<syntax:y5>))" +
            ")");
    }

    static void test_dup() {
        Object form = Parser.parse1("(1 (2 3))");
        Syntax s = Syntax.fromDatum(null, form);

        expectEquals(Pattern.ofStr("(a a)").match(form).get("a"), 1);
        expectEquals(Pattern.ofStr("(a a)").match(s).get("a"), Syntax.of(1));

        expectEquals(match("(a a)", form).toString(),
                "((a 1) (a (2 3)))");

        expectEquals(match("(a a)", s).toString(),
                "((a #<syntax:1>) (a #<syntax:(2 3)>))");
    }


    static void test_makeEmptyVars() {
        Matcher m = new Matcher(null, null, new String[0]);
        expectEquals(
                m.makeEmptyVars(Parser.parse1("(((a ...) ...) ...)")),
                RT.list(RT.list(RT.sym("a"), RT.Null()))
        );
        expectEquals(
                m.makeEmptyVars(Parser.parse1("(a (b) c)")),
                RT.list(
                        RT.list(RT.sym("a"), RT.Null()),
                        RT.list(RT.sym("b"), RT.Null()),
                        RT.list(RT.sym("c"), RT.Null())
                )
        );
        expectEquals(
                m.makeEmptyVars(Parser.parse1("(a (b ...) ...)")),
                RT.list(
                        RT.list(RT.sym("a"), RT.Null()),
                        RT.list(RT.sym("b"), RT.Null())
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
            Syntax s = Syntax.of(RT.cons(Syntax.of(1), Syntax.of(2)));
            m.toSyntaxList(s);
            throw new RuntimeException();
        } catch (InterpError ignored) {}
    }

    static void test_toSyntaxList() {
        Matcher m = new Matcher(Syntax.of(null), "()", new String[0]);
        PList s123 = RT.list(Syntax.of(1), Syntax.of(2), Syntax.of(3));

        {
            // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
            Syntax s = Syntax.of(
                    RT.cons(
                            Syntax.of(1),
                            Syntax.of(RT.list(Syntax.of(2), Syntax.of(3)))
                    )
            );
            PList sl = m.toSyntaxList(s);
            expectEquals(s123, sl);
        }
        {
            // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
            Syntax s = Syntax.of(
                    RT.cons(
                            Syntax.of(1),
                            RT.cons(
                                    Syntax.of(2),
                                    RT.cons(
                                            Syntax.of(3),
                                            Syntax.of(RT.Null())
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
            PList expr = RT.list(RT.sym("+"), 1, 1);
            Syntax s = Syntax.fromDatum(null,
                    RT.list(
                            RT.sym("define"),
                            RT.sym("a"),
                            expr
                    ));
            {
                Finder r = of1(Parser.parse1("(define id expr)")).match(s);
                expectEquals(r.get("expr"), Syntax.fromDatum(null, expr));
            }
            {
                Finder r = of2(Parser.parse1("(define id expr)")).match(s);
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
                        RT.list(
                                RT.list(a, b),
                                RT.list(c, d)
                        )
                );
                expectEquals(r.get("rhs"),
                        RT.list(
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
                        RT.list(
                                RT.list(a, b),
                                RT.list(c, d)
                        )
                );
                expectEquals(r.get("rhs"),
                        RT.list(
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
                "name", RT.sym("a"),
                "val", RT.list(RT.sym("+"), 1, 2));

        assert1("a", new String[0], "1", true, "a", 1);
        assert1("(a)", new String[0], "(1)", true, "a", 1);
        assert1("(a b)", new String[0], "(1 2)", true,
                "a", 1, "b", 2);
        assert1("(a b)", new String[0], "(1 (2 3))", true,
                "a", 1, "b", RT.list(2, 3));

        assert1("([var init step ...] ...)", new String[0], "((a 0) (b 0 1) (c 0 1 2))", true,
                "var", lists(RT.sym("a"), RT.sym("b"), RT.sym("c")),
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

    static void expectEquals(Object a, Object b, String ...msg) {
        if (!Objects.equals(a, b)) {
            throw new AssertionError(Arrays.toString(msg));
        }
    }

    static void expectTrue(boolean r, String ...msg) {
        if (!r) {
            throw new AssertionError(Arrays.toString(msg));
        }
    }

    @SafeVarargs
    static <E> List<E> lists(E... els) {
        return ((List<E>) RT.list(els));
//        if (els.length == 0) {
//            return Collections.emptyList();
//        } else if (els.length == 1) {
//            return Collections.singletonList(els[0]);
//        } else {
//            List<E> lst = new ArrayList<>();
//            Collections.addAll(lst, els);
//            return lst;
//        }
    }

}
