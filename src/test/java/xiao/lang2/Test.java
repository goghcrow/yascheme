package xiao.lang2;

import xiao.lang2.expander2.UnitTest;

import java.util.List;

import static xiao.lang2.Misc.resource;

/**
 * @author chuxiaofeng
 */
public class Test {

    public static void main(String[] args) {
        test_parser();
        TestPatternMatch.test();
        UnitTest.test();
        TestExpand2.test();
        TestExpand2.main(args);

        // debug(resource("/pure.ss")); System.exit(0);
        // Interp.interp(resource("/bug.ss"));
        Interp.interp(resource("/java-interop.ss"));
        Interp.interp(resource("/test.ss"));
        Interp.interp(resource("/tmp.ss"));
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
