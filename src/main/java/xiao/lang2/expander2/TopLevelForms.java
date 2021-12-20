package xiao.lang2.expander2;

import xiao.lang2.InterpError;

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
