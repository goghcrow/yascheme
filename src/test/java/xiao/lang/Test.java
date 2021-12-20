package xiao.lang;

import xiao.lang.expander.Expander;
import xiao.lang.expander.UnitTest;

import java.util.List;

import static xiao.lang.Misc.resource;
import static xiao.lang.expander.Expander.eval;

/**
 * @author chuxiaofeng
 */
public class Test {

    @org.junit.Test
    public void cover() {
        Test.main(new String[0]);
    }

    public static void main(String[] args) {
        test_parser();
        TestPatternMatch.test();
        UnitTest.test();
        TestExpand.test();
        test_expand();

        // debug(resource("/pure.ss")); System.exit(0);
        // Interp.interp(resource("/bug.ss"));
        Interp.interp(resource("/java-interop.ss"));
        Interp.interp(resource("/test.ss"));
        Interp.interp(resource("/tmp.ss"));
    }

    static void test_expand() {
        // xiao.lang.Test.debug(resource("/core/boot0.ss"));
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
        System.out.println("expand: " + TestExpand.expand(s));

        System.out.println("compile: " + TestExpand.expandAndCompile(s));
        System.out.println("eval: " + eval(s));
    }

    static void test_parser() {
        List<Object> parse = Parser.parse(TestMisc.resource("/test-parser.ss"));
        System.out.println(parse);
    }

    static void debug(String s) {
        Interp interp = new Interp(false);
        Env env = new Env();
        Procedures.init(interp, env);
        Syntaxes.init(interp, env);
        Object form = Reader.read(s);
        interp.interp(form, env.derive(), System.out::println);
    }
}
