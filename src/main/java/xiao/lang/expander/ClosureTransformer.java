package xiao.lang.expander;

import xiao.lang.Env;
import xiao.lang.Interp;

import static xiao.lang.Procedures.Closure;
import static xiao.lang.Values.Procedure;

/**
 * 通过 define-syntaxes\letrec-syntaxes+values 定义的 transformer
 * (lambda (stx) stx)
 * @author chuxiaofeng
 */
public class ClosureTransformer implements Procedure  {
    final Closure closure;

    public ClosureTransformer(Closure closure) {
        this.closure = closure;
    }

    @Override
    public void call(Object[] args, Env E, Interp.Ctx K) {
        closure.call(args, E, K);
    }

    @Override
    public String toString() {
        return "#<transformer>";
    }
}
