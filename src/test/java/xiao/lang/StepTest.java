package xiao.lang;

import xiao.lang.expander.CompiledExpression;
import xiao.lang.expander.Expander;
import xiao.lang.expander.Namespace;
import xiao.lang.expander.Syntax;

import java.util.List;

public class StepTest {

    final static Expander expander = Expander.of();

    static void stepTest(String s) {
        List<Object> parsed = Parser.parse(s);
        System.out.println("parse: " + parsed);

        Object form = Reader.read(s);
        System.out.println("read: " + form);

        Namespace ns = expander.currentNamespace();

        Syntax expanded = expander.expand(form, ns);
        System.out.println("expand: " + expanded);

        CompiledExpression compiled = expander.compile(expanded, ns);
        System.out.println("compile: " + compiled);

        System.out.println("eval: " + expander.eval(compiled));
    }

    public static void main(String[] args) {
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
}
