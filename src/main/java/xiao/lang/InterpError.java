package xiao.lang;

/**
 * @author chuxiaofeng
 */
public class InterpError extends RuntimeException {

    public InterpError(String msg) {
        super(msg);
    }

    public InterpError(String fmt, Object... args) {
        super(String.format(fmt.replace("#%", "#％"), args));
    }

}
