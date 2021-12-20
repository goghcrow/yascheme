package xiao.lang2.expander2;

/**
 * @author chuxiaofeng
 */
public interface SyntaxTransformer {

    Syntax transform(Syntax s, ExpandContext ctx);

}
