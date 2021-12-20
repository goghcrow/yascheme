package xiao.lang2;

import xiao.lang2.expander2.Expander;
import xiao.lang2.expander2.Namespace;
import xiao.lang2.expander2.Syntax;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Objects;

import static xiao.lang2.Misc.resource;
import static xiao.lang2.Procedures.sym;
import static xiao.lang2.Values.Procedure;
import static xiao.lang2.expander2.Expander.eval;

/**
 * @author chuxiaofeng
 */
public class TestExpand2 {

    static Values.PList list(Object... args) {
        return Procedures.list(args);
    }

    public static void test() {
        TestExpand2 self = new TestExpand2();

        self.test_lambda();
        self.test_caseLambda();
        self.test_defineValues();
        self.test_defineValues1();
        self.test_defineSyntaxes();
        self.test_letrecSyntaxesValues();
        self.test_letrecSyntaxesValues_defineValues();
        self.test_expansion_not_captured();
        self.test_non_capturing_expansion();


        self.test_distinct_generated_variables();
        self.test_use_site_scopes();
        self.test_use_site_scope_remove_from_binding_position();
        self.test_non_transformer_binding_misuse();
    }

    // 恢复 junit 时候注释掉即可
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD})
    public @interface Test {
        Class<? extends Throwable> expected() default None.class;
        class None extends Throwable { }
    }


    public static void main(String[] args) {
        // xiao.lang2.Test.debug(resource("/core/boot0.ss"));
        {
            Object form = Reader.read(resource("/xxx.ss"));
            Expander.CompiledExpression s = Expander.compileWithSyntax(form);
             System.out.println(PrettyPrint.pp(s.sexpr));
            System.out.println(s.sexpr);
            System.out.println(eval(s));
        }
//                System.exit(0);
        {
            Object form = Reader.read(resource("/xx.ss"));
            Expander.CompiledExpression s = Expander.compileWithSyntax(form);
//                         System.out.println(PrettyPrint.pp(s.sexpr));
            System.out.println(s);
            System.out.println(eval(s));
        }
        {
            Object form = Reader.read(resource("/java-interop.ss"));
            Expander.CompiledExpression s = Expander.compileWithSyntax(form);
            System.out.println(s);
            System.out.println(eval(s));
        }
        {
            Object form = Reader.read(resource("/test.ss"));
            Expander.CompiledExpression s = Expander.compileWithSyntax(form);
            System.out.println(s);
            System.out.println(eval(s));
        }
        {
            Object form = Reader.read(resource("/tmp.ss"));
            Expander.CompiledExpression s = Expander.compileWithSyntax(form);
            System.out.println(s);
            System.out.println(eval(s));
        }


        stepTest("((lambda () '(1 1)))");
        stepTest("((lambda () (list 1 1)))");


        ///////////////////////////////////


        // dot (. instance-expr (method-symbol args ...))
        stepTest("(. \"Hello\" (substring  1))");

        // dot (. instance-expr method-symbol args ...)
        stepTest("(. \"Hello\" substring  1)");

        // dot (. instance-expr member-symbol)
        stepTest("(. 42 value)");

        // dot (. instance-expr -field-symbol)
        stepTest("(. 42 -value)");

        // dot (. ClassName-symbol member-symbol)
        stepTest("(. java.lang.System out)");

        // dot (. ClassName-symbol (method-symbol args ...))
        stepTest("(. java.lang.Integer (parseInt \"42\"))");

        // dot (. ClassName-symbol method-symbol args ...)
        stepTest("(. java.lang.Integer parseInt \"42\")");


        /////////////////////////////////////////////////////////////////////////////////////

        // reader: ClassName/staticField ==> (. ClassName staticField)
        stepTest("java.lang.System/out");

        // (ClassName/staticMethod args ...) ==> (. ClassName-symbol (method-symbol args ...))
        stepTest("(java.lang.Integer/parseInt \"42\")");

        // (.-instanceField instance-expr) ==> (. instance-expr -instanceField)
        stepTest("(.-value 42)");

        // (.instanceMember instance-expr args ...) ==> (. instance-expr instanceMember args ...)
        stepTest("(.substring \"Hello\" 1)");

        // (.instanceMember ClassName args ...) ==> (. ClassName instanceMember args ...)
        stepTest("(.parseInt java.lang.Integer \"42\")");

        // (ClassName. args ...) ==> (new ClassName args ...)
        stepTest("(java.lang.String. \"a\")");
        stepTest("(java.lang.Integer. 42)");
    }

    static void stepTest(String s) {
        System.out.println("parse: " + Parser.parse(s));
        System.out.println("read: " + Reader.read(s));
        System.out.println("expand: " + expand(s));

        System.out.println("compile: " + expandAndCompile(s));
        System.out.println("eval: " + eval(s));
    }

    @Test
    public void test_lambda() {
        expect(eval("((lambda (a . rest) rest) 0 1 2 3)"), list(1, 2, 3));

        expect(expandAndCompile("(lambda () (list 1 1))").toString(),
                "(lambda () (#<procedure:list> '1 '1))");

        expect(eval("((lambda () (list 1 1)))"), list(1, 1));
    }

    @Test
    public void test_caseLambda() {
        String s = "(case-lambda\n" +
                "   [(x) (set! x 5)]\n" +
                "   [(x y) (begin0 y x)]\n" +
                "   [() (list 1 2 3)])";
        System.out.println(expandAndCompile(s));
        // (case-lambda ((xϟ2)      (set! xϟ2      (quote 5))) ((xϟ3 yϟ4)           (begin0 yϟ4 xϟ3))           (() (#<procedure:list> (quote 1) (quote 2) (quote 3))))
        // (case-lambda ((x1448444) (set! x1448444 (quote 5))) ((x1448445 y1448446) (begin0 y1448446 x1448445)) (() (#<procedure:list> (quote 1) (quote 2) (quote 3))))

        Syntaxes.CaseLambda.CaseClosure caseClosure =
                ((Syntaxes.CaseLambda.CaseClosure) eval(s));
        expect(callProcedure(caseClosure), list(1, 2, 3));
        expect(callProcedure(caseClosure, 1, 2), 2);
        expect(callProcedure(caseClosure, 1), Void.TYPE);
    }

    @Test
    public void test_defineValues() {
        String s = "((lambda (x) (define-values (y) x) y) 1)";
        expect(
                expand(s).toString(),
                "#<syntax:(#%app (lambda (x) (letrec-values (((y) x)) y)) '1)>"
        );
        System.out.println(expandAndCompile(s));
        // ((lambda (xϟ4)      (letrec-values ((yϟ5) xϟ4) yϟ5))                  (quote 1))
        // ((lambda (x1451027) (letrec-values (((y1451028) x1451027)) y1451028)) (quote 1))

        expect(eval(s), 1);
    }

    @Test
    public void test_defineValues1() {
        String s1 = "((lambda (x) 42 100 (define-values (y) x) y) 1)";
        String s2 = "((lambda (x) (define-values (y) x) 42 100 y) 1)";
        expect(
                expand(s1).toString(),
                "#<syntax:(#%app (lambda (x) (letrec-values ((() (begin '42 (#%app values))) (() (begin '100 (#%app values))) ((y) x)) y)) '1)>"
        );
        expect(
                expand(s2).toString(),
                "#<syntax:(#%app (lambda (x) (letrec-values (((y) x)) (begin '42 '100 y))) '1)>"
        );
        System.out.println(expandAndCompile(s1));
        // ((lambda (xϟ2) (letrec-values ((() (begin '42 (#<procedure:values>))) (() (begin '100 (#<procedure:values>))) ((yϟ3) xϟ2)) yϟ3)) '1)
        System.out.println(expandAndCompile(s2));
        // ((lambda (xϟ2) (letrec-values (((yϟ3) xϟ2)) (begin '42 '100 yϟ3))) '1)

        expect(eval(s1), 1);
        expect(eval(s2), 1);

        try {
            expandAndCompile("(lambda () (define-values (y y) (values x x)) y)");
            throw new RuntimeException();
        } catch (InterpError e) {
            expect(e.getMessage(), "duplicate binding: #<syntax:y>");
        }
        try {
            expandAndCompile("(lambda () (define-values (y) x) (define-values (y) x) y)");
            throw new RuntimeException();
        } catch (InterpError e) {
            expect(e.getMessage(), "duplicate binding: #<syntax:y>");
        }
    }

    @Test
    public void test_defineSyntaxes() {
        String s = "(lambda (x)\n" +
                "   (define-syntaxes (y) (lambda (stx) (quote-syntax 7)))\n" +
                "   y)";
        expect(
                expand(s).toString(),
                "#<syntax:(lambda (x) '7)>"
        );

        System.out.println(expandAndCompile(s));
        // (lambda (xϟ5)      (quote 7))
        // (lambda (x1453563) (quote 7))

        expect(
                eval("(" + s + " 0)"),
                7
        );
    }

    @Test
    public void test_letrecSyntaxesValues() {
        String s = "(let-values ([(z) 9])\n" +
                "   (letrec-syntaxes+values\n" +
                "    ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])\n" +
                "    ([(x) 5] [(y) (lambda (z) z)])\n" +
                "    (let-values ([(z) 10])\n" +
                "      (begin z (if (m 10) 1 2)))))";
        expect(
                expand(s).toString(),
                "#<syntax:(let-values (((z) '9)) (letrec-values (((x) '5) ((y) (lambda (z) z))) (let-values (((z) '10)) (begin z (if '10 '1 '2)))))>"
        );

        System.out.println(expandAndCompile(s));
        // matt 的实现有 bug, begin 内容顺序反了
        // (let-values (((z1429774) (quote 9))) (letrec-values (((x1429776) (quote 5)) ((y1429777) (lambda (z1429831) z1429831))) (let-values (((z1429832) (quote 10))) (begin (if (quote 10) (quote 1) (quote 2)) z1429832))))
        // (let-values (((zϟ9)      (quote 9))) (letrec-values (((xϟ11)     (quote 5)) ((yϟ12)     (lambda (zϟ14) zϟ14)))         (let-values (((zϟ15)     (quote 10))) (begin zϟ15 (if (quote 10) (quote 1) (quote 2))))))

        // expect(eval(s), 10);
        expect(eval(s), 1);
    }

    @Test
    public void test_letrecSyntaxesValues_defineValues() {
        String s = "(let-values ([(z) 9])\n" +
                "   (letrec-syntaxes+values\n" +
                "    ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])\n" +
                "    ([(x) 5]\n" +
                "     [(y) (lambda (z) z)])\n" +
                "    (let-values ([(z) 10])\n" +
                "      (define-values (x y z) (values 1 2 3))\n" +
                "      (begin\n" +
                "        z\n" +
                "        (if (m x) y z)))))";
        System.out.println(expand(s));
        System.out.println(expandAndCompile(s));
        expect(eval(s), 2);
    }

    @Test
    public void test_expansion_not_captured() {
        // "expansion not captured"
        String s = "(let-values ([(x) 'x-1])\n" +
                "   (letrec-syntaxes+values\n" +
                "    ([(m) (lambda (stx) (quote-syntax x))])\n" +
                "    ()\n" +
                "    (let-values ([(x) 'x-3])\n" +
                "      (m))))";
        expect(
                expand(s).toString(),
                "#<syntax:(let-values (((x) 'x-1)) (letrec-values () (let-values (((x) 'x-3)) x)))>"
        );

        System.out.println(expandAndCompile(s));
        // (let-values (((xϟ6)      (quote x-1))) (letrec-values () (let-values (((xϟ9)      (quote x-3))) xϟ6)))
        // (let-values (((x1456162) (quote x-1))) (letrec-values () (let-values (((x1456217) (quote x-3))) x1456162)))

        expect(eval(s), sym("x-1"));
    }

    @Test
    public void test_non_capturing_expansion() {
        // "non-capturing expansion"
        String s = "(let-values ([(x) 'x-1])\n" +
                "   (letrec-syntaxes+values\n" +
                "    ([(m) (lambda (stx)\n" +
                "            (datum->syntax\n" +
                "             #f\n" +
                "             (list (quote-syntax let-values)\n" +
                "                   (list (list (list (quote-syntax x))\n" +
                "                               (quote-syntax 'x-2)))\n" +
                "                   (car (cdr (syntax-e stx))))))])\n" +
                "    ()\n" +
                "    (let-values ([(x) 'x-3])\n" +
                "      (m x))))";
        expect(
                expand(s
                        ).toString(),
                "#<syntax:(let-values (((x) 'x-1)) (letrec-values () (let-values (((x) 'x-3)) (let-values (((x) 'x-2)) x))))>"
        );

        System.out.println(expandAndCompile(s));
        // (let-values (((xϟ7)      (quote x-1))) (letrec-values () (let-values (((xϟ10)     (quote x-3))) (let-values (((xϟ11)     (quote x-2))) xϟ10))))
        // (let-values (((x1458767) (quote x-1))) (letrec-values () (let-values (((x1458822) (quote x-3))) (let-values (((x1458823) (quote x-2))) x1458822))))

        expect(eval(s), sym("x-3"));
    }

    @Test
    public void test_distinct_generated_variables() {
        // "distinct generated variables"
        String s = "(letrec-syntaxes+values\n" +
                "   ([(gen) (lambda (stx)\n" +
                "             (let-values ([(vals) (syntax-e (car (cdr (syntax-e stx))))]\n" +
                "                          [(binds) (syntax-e (car (cdr (cdr (syntax-e stx)))))]\n" +
                "                          [(refs) (syntax-e (car (cdr (cdr (cdr (syntax-e stx))))))])\n" +
                "               (datum->syntax\n" +
                "                #f\n" +
                "                (if (null? vals)\n" +
                "                    (list (quote-syntax bind) binds refs)\n" +
                "                    (list (quote-syntax gen)\n" +
                "                          (cdr vals)\n" +
                "                          (cons (list (list (quote-syntax x))\n" +
                "                                      (car vals))\n" +
                "                                binds)\n" +
                "                          (cons (quote-syntax x)\n" +
                "                                refs))))))]\n" +
                "    [(bind) (lambda (stx)\n" +
                "              (let-values ([(binds) (car (cdr (syntax-e stx)))]\n" +
                "                           [(refs) (car (cdr (cdr (syntax-e stx))))])\n" +
                "                (datum->syntax\n" +
                "                 (quote-syntax here)\n" +
                "                 (list (quote-syntax let-values)\n" +
                "                       binds\n" +
                "                       (cons (quote-syntax list)\n" +
                "                             refs)))))])\n" +
                "   ()\n" +
                "   (gen (1 2) () ()))";

        expect(
                expand(s).toString(),
                "#<syntax:(letrec-values () (let-values (((x) '2) ((x) '1)) (#%app list x x)))>"
        );
        System.out.println(expandAndCompile(s));
        // (letrec-values () (let-values (((xϟ22)     (quote 2)) ((xϟ23)     (quote 1))) (#<procedure:list> xϟ22     xϟ23)))
        // (letrec-values () (let-values (((x1511028) (quote 2)) ((x1511029) (quote 1))) (#<procedure:list> x1511028 x1511029)))

        expect(eval(s), list(2, 1));
    }

    @Test
    public void test_use_site_scopes() {
        // "use-site scopes (so not ambiguous)"
        String s = "((let-values ()\n" +
                "     (define-syntaxes (identity)\n" +
                "       (lambda (stx)\n" +
                "         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])\n" +
                "           (datum->syntax\n" +
                "            (quote-syntax here)\n" +
                "            (list 'lambda '(x)\n" +
                "                  (list 'let-values (list\n" +
                "                                     (list (list misc-id) ''other))\n" +
                "                        'x))))))\n" +
                "     (identity x))\n" +
                "   'ok)";
        System.out.println(expandAndCompile(s));
        // ((let-values () (lambda (xϟ15)     (let-values (((xϟ16)     (quote other))) xϟ15)))     (quote ok))
        // ((let-values () (lambda (x1769247) (let-values (((x1769248) (quote other))) x1769247))) (quote ok))

        expect(eval(s), sym("ok"));
    }

    @Test
    public void test_use_site_scope_remove_from_binding_position() {
        // "use-site scope remove from binding position"
        String s = "(let-values ()\n" +
                "   (define-syntaxes (define-identity)\n" +
                "     (lambda (stx)\n" +
                "       (let-values ([(id) (car (cdr (syntax-e stx)))])\n" +
                "         (datum->syntax\n" +
                "          (quote-syntax here)\n" +
                "          (list 'define-values (list id) '(lambda (x) x))))))\n" +
                "   (define-identity f)\n" +
                "   (f 'still-ok))";
        System.out.println(expandAndCompile(s));
        // (let-values () (letrec-values (((fϟ5)      (lambda (xϟ6)      xϟ6     ))) (fϟ5      (quote still-ok))))
        // (let-values () (letrec-values (((f1765056) (lambda (x1765057) x1765057))) (f1765056 (quote still-ok))))

        expect(eval(s), sym("still-ok"));
    }

    @Test
    public void test_non_transformer_binding_misuse() {
        // "non-transformer binding misuse"
        try {
            expandAndCompile(
                    "(letrec-syntaxes+values\n" +
                            "                       ([(v) 1])\n" +
                            "                       ()\n" +
                            "                       v)"
            );
            throw new RuntimeException("shouldn't get here");
        } catch (InterpError e) {
            expect(e.getMessage().contains("illegal use of syntax"), true);
        }
    }

    static Object callProcedure(Procedure c, Object... args) {
        Object[] ref = new Object[1];
        Interp.run(() -> c.call(
                args,
                new Env(), // 闭包用不到 call scope
                v -> ref[0] = v
        ));
        return ref[0];
    }

    static Syntax expand(String s) {
        Object form = Reader.read(s);
        Namespace ns = Namespace.currentNamespace();
        // expand+compile+eval-expression
        return Expander.expand(form, ns);
    }

    static Object expandAndCompile(String s) {
        Syntax expanded = expand(s);
        Namespace ns = Namespace.currentNamespace();
        Expander.CompiledExpression compiled = Expander.compile(expanded, ns);
        return compiled.sexpr;
    }

    static void expect(Object actual, Object expected) {
        System.out.println("----------------------------------------------------");
        System.out.println("actual:   " + actual);
        System.out.println("expected: " + expected);
        System.out.println("----------------------------------------------------\n");

        // Assert.assertEquals(expected, actual);

        if (!Objects.equals(actual, expected)) {
            throw new IllegalStateException("expected: " + expected + ", actual: " + actual);
        }
    }
}
