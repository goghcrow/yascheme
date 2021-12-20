package xiao.lang.expander;

import static xiao.lang.Values.Symbol;

/**
 * A binding is either a
 *      - symbol = core form or primitives
 *      - gensym = local binding
 *
 * @author chuxiaofeng
 */
public interface Binding {

    // Returns `variable` or a compile-time value
    // 'variable | xiao.lang.Values.Closure | xiao.lang.expander.SyntaxTransformer
    Object lookup(ExpandContext ctx, Syntax id);

    Symbol symbol();

}
