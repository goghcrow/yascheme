package xiao.lang.expander;

import xiao.lang.Pattern;

import static xiao.lang.Pattern.Finder;

/**
 * @author chuxiaofeng
 */
public interface SyntaxMatcher {

    static Finder match(String pattern, Object inputForm, String ...formals) {
        return Pattern.ofStr(pattern, formals).match(inputForm);
    }

    static Finder match(Object pattern, Object inputForm, String ...formals) {
        return Pattern.of(pattern, formals).match(inputForm);
    }

    static Finder tryMatch(String pattern, Object inputForm, String ...formals) {
        return Pattern.ofStr(pattern, formals).tryMatch(inputForm);
    }

    static Finder tryMatch(Object pattern, Object inputForm, String ...formals) {
        return Pattern.of(pattern, formals).tryMatch(inputForm);
    }
}
