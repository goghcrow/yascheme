package xiao.lang2.expander2;

import static xiao.lang2.Pattern.Finder;
import static xiao.lang2.Pattern.ofStr;
import static xiao.lang2.Procedures.sym;
import static xiao.lang2.Values.Symbol;

/**
 * @author chuxiaofeng
 */
public class Utils {

    static int cnt_ = 0;

    static Symbol gensym(String prefix) {
        return sym(prefix + "ϟ" + ++cnt_);
    }

    static Symbol gensym(Symbol prefix) {
        return gensym(prefix.name);
    }

    static Finder match(String pattern, Object inputForm) {
        return match(pattern, new String[0], inputForm);
    }

    static Finder match(String pattern, String[] formals, Object inputForm) {
        return ofStr(pattern, formals).match(inputForm);
    }

    static Finder tryMatch(String pattern, Object inputForm) {
        return tryMatch(pattern, new String[0], inputForm);
    }

    static Finder tryMatch(String pattern, String[] formals, Object inputForm) {
        return ofStr(pattern, formals).tryMatch(inputForm);
    }

}
