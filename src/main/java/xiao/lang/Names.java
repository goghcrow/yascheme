package xiao.lang;

/**
 * @author chuxiaofeng
 */
public interface Names {
    String KEYWORD_PREFIX = "#:";
    String ELLIPSIS = "...";
    String ELLIPSIS_PLUS = "...+";
    String UNDERSCORE = "_";

    String HYPHEN = "-";
    String DOT = ".";
    String DOT_DOT = "..";
    String SLASH = "/";

    String BEGIN = "begin";
    String BEGIN0 = "begin0";

    String LAMBDA0 = "λ";
    String LAMBDA = "lambda";
    String CASE_LAMBDA = "case-lambda";
    String IF = "if";
    String CALLCC = "call-with-current-continuation";

    String NEW = "new";
    String EVAL = "eval";
    String APPLY = "apply";

    String QUOTE = "quote";
    String QUASIQUOTE = "quasiquote";
    String UNQUOTE = "unquote";
    String UNQUOTE_SPLICING = "unquote-splicing";

    String SYNTAX_RULES = "syntax-rules";

    String DEFINE = "define";
    String SET = "set!";

    String TRUE = "#t";
    String FALSE = "#f";

    String VALUES = "values";
    String CALL_WITH_VALUES = "call-with-values";
    String DEFINE_VALUES = "define-values";
    String LET_VALUES = "let-values";
    String LETREC_VALUES = "letrec-values";

    String MATCH_SYNTAX = "match-syntax";
    String TRY_MATCH_SYNTAX = "try-match-syntax";
}
