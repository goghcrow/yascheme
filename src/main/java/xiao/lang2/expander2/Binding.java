package xiao.lang2.expander2;

import static xiao.lang2.Values.Symbol;

/**
 * A binding is either a
 *      - symbol = core form or primitives
 *      - gensym = local binding
 *
 * @author chuxiaofeng
 */
public interface Binding {

    // Returns `variable` or a compile-time value
    // 'variable | xiao.lang.Values.Closure | xiao.lang.expander2.SyntaxTransformer
    Object lookup(ExpandContext ctx, Syntax id);

    Symbol symbol();

}
