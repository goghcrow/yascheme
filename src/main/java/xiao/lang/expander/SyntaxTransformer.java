package xiao.lang.expander;

/**
 * @author chuxiaofeng
 */
public interface SyntaxTransformer {

    Syntax transform(Syntax s, ExpandContext ctx);

}
