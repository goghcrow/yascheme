package xiao.lang;

import xiao.lang.expander.*;

import java.io.IOException;
import java.lang.reflect.Array;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

import static xiao.lang.Misc.*;
import static xiao.lang.RT.list;
import static xiao.lang.RT.sym;

/**
 * @author chuxiaofeng
 */
public class Test {

    // todo 检查 expander 的代码覆盖率
    @org.junit.Test
    public void cover() {
        Test.main(new String[0]);
    }

    static void test_without_syntaxes(String s) {
        Expander expander = Expander.of();
        Namespace ns = expander.currentNamespace();
        CompiledExpression compiled = expander.compile(expander.expand(list(
                sym("let-values"),
                list(),
                Reader.read(s)
        ), ns), ns);
//        System.out.println(Interp.pp(compiled.sexpr));
        Object eval = compiled.eval();
        System.out.println(eval);
    }

    static void test_with_syntaxes(String s) {
        CompiledExpression compiled = Interp.compile(s);
//        System.out.println(Interp.pp(compiled.sexpr));
        System.out.println(compiled.eval());
    }

    public static void main(String[] args) {
//        test_with_syntaxes(resource("/tmp.ss"));System.exit(0);
        test_parser();
        TestPatternMatch.main(args);
        UT.main(args);
        TestExpand.main(args);

        test_expand();
    }

    static void test_parser() {
        List<Object> parse = Parser.parse(resource("/test-parser.ss"));
        System.out.println(parse);
    }

    static void test_expand() {
        testFile("/r5rs.ss");
        testFile("/r7rs.ss");

        testFile("/concept.ss");
        testFile("/amb.ss");
        testFile("/callcc.ss");
        testFile("/expansion.ss");
        testFile("/interop.ss");
        testFile("/macro.ss");
        testFile("/test.ss");
        testFile("/trace-test.ss");
        testFile("/tmp.ss");
    }

    static void testFile(String path) {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TEST " + path);
        CompiledExpression compiled = Interp.compile(pathOfRes(path));
        // System.out.println(PrettyPrint.pp(compiled.sexpr));
        // System.out.println(compiled.sexpr);
        System.out.println(compiled.eval());
    }


    static String resource(String path) {
        try {
            URL url = Test.class.getResource(path);
            if (url == null) {
                throw new RuntimeException("文件不存在: " + path);
            }
            return new String(Files.readAllBytes(Paths.get(url.toURI())));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    // 不要删, 给 ss 单测用的
    static long[] varArgsTest(long ...a) { return a; }
    static int[] varArgsTest(int ...a) { return a; }
    static int[] varArgsTest(String s, int ...a) { return a; }
}
