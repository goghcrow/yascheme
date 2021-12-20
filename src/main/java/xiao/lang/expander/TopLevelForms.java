package xiao.lang.expander;

import xiao.lang.InterpError;

/**
 * expand-top-level
 * @author chuxiaofeng
 */
public interface TopLevelForms {

    static Syntax defineValues(Syntax s, ExpandContext ctx) {
        throw new InterpError("not allowed in an expression position: " + s);
    }

    static Syntax defineSyntaxes(Syntax s, ExpandContext ctx) {
        throw new InterpError("not allowed in an expression position: " + s);
    }

}
