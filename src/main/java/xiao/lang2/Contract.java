package xiao.lang2;

/**
 * @author chuxiaofeng
 */
public interface Contract {

    // require
    static void expect(boolean test) {
        if (!test) {
            throw new InterpError("contract violation");
        }
    }

    static void expect(boolean test, String msg) {
        if (!test) {
            throw new InterpError(msg);
        }
    }

    static void expect(boolean test, String fmt, Object... args) {
        if (!test) {
            throw new InterpError(fmt, args);
        }
    }

}
