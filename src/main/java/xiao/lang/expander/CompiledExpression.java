package xiao.lang.expander;

/**
 * @author chuxiaofeng
 */
public class CompiledExpression {
    final Expander expander;
    public final Object sexpr;

    public CompiledExpression(Expander expander, Object sexpr) {
        this.expander = expander;
        this.sexpr = sexpr;
    }

    public Object eval() {
        return expander.eval(this);
    }

    @Override
    public String toString() {
        return "#<compiled-expression:" + sexpr + ">";
    }
}
