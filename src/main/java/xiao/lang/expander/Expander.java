package xiao.lang.expander;

import xiao.lang.*;

import static xiao.lang.Misc.resource;
import static xiao.lang.Procedures.list;
import static xiao.lang.Procedures.sym;
import static xiao.lang.Values.Callable;
import static xiao.lang.Values.Procedure;


/**
 * eval 等方法转移到 interp 中
 * 健康宏展开 (根据马晓的 Youtube 演讲改写)
 *
 * 演讲视频 https://www.youtube.com/watch?v=Or_yKiI3Ha4
 * 演讲ppt https://my.eng.utah.edu/~cs3520/f19/lecture27.pdf
 * racket代码 https://github.com/mflatt/expander
 * racket 文档: https://docs.racket-lang.org/reference/syntax-model.html
 * @author chuxiaofeng
 */
public class Expander {

    public static Object eval(String form) {
        // todo 这里 read 重复
        return eval(Reader.read(form), Namespace.currentNamespace());
    }

    public static Object eval(Object form) {
        return eval(form, Namespace.currentNamespace());
    }

    public static Object eval(Object form, Namespace ns) {
        // expand+compile+eval-expression
        Syntax expanded = expand(form, ns);
        CompiledExpression compiled = compile(expanded, ns);
        return eval(compiled);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // step api

    // import core bindings
    // The `namespace-syntax-introduce` function adds the core scope to a
    // syntax object; it needs to be used, for example, on a just-created
    // syntax object to make `lambda` refer to the core lambda form
    public static Syntax namespaceSyntaxIntroduce(Object s) {
        // All top-level bindings are in the core scope:
        // ~~The only initial bindings are in the core scope~~
        return ((Syntax) Scope.add(s, Core.coreScope));
    }

    // expand-expression
    public static Syntax expand(String form, Namespace ns) {
        return expand(Reader.read(form), ns);
    }

    // expand-expression
    public static Syntax expand(Object form, Namespace ns) {
        // convert to syntax-object
        Object syntax = Syntax.fromDatum(null, form);
        // add core-scope
        Syntax introSyntax = namespaceSyntaxIntroduce(syntax);
        return expand(introSyntax, ns);
    }

    public static Syntax expand(Syntax s, Namespace ns) {
        ExpandContext ctx = ExpandContext.makeExpandContext(ns);
        return expandInContext(s, ctx);
    }

    public static CompiledExpression compile(Syntax s, Namespace ns) {
        return new CompiledExpression(compileInNamespace(s, ns));
    }

    public static Object eval(CompiledExpression form) {
        return runTimeEval(form.sexpr);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    private static Syntax expandInContext(Syntax s, ExpandContext ctx) {
        return Expansion.expand(s, ctx);
    }

    private static Object compileInNamespace(Syntax s, Namespace ns) {
        return Compiler.compile(s, ns);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    public static class CompiledExpression {
        public final Object sexpr;

        public CompiledExpression(Object sexpr) {
            this.sexpr = sexpr;
        }

        @Override
        public String toString() {
            return "#<compiled-expression:" + sexpr + ">";
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    public static Object expandTimeEval(Object compiled) {
        return eval(interp_forExpand, compiled, env_forExpand.derive());
    }

    public static Object runTimeEval(Object compiled) {
        return eval(interp_forRun, compiled, env_forRun.derive());
    }

    static Object eval(Interp interp, Object s, Env env) {
        Object[] ref = new Object[1];
        interp.interp(Reader.read(s), env, v -> ref[0] = v);
        return ref[0];
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // todo !!! 注意 static 顺序
    // todo 程序改成 程序嵌套 (print-values)

    final static Interp interp_forCurrentNamespace = new Interp(false);
    final static Interp interp_forExpand = new Interp(false);
    final static Interp interp_forRun = new Interp(false);

    final static Env env_forCurrentNamespace = new Env();
    final static Env env_forExpand = new Env(); // expand-time-namespace
    final static Env env_forRun = new Env(); // run-time-namespace

    static {
        initCurrentNamespaceEnv();
        initCurrentNamespace();
        initExpandEnv();
        initRunTimeEnv();
        require("/core/boot1.ss");
    }

    static void initCurrentNamespaceEnv() {
        Procedures.init(interp_forCurrentNamespace, env_forCurrentNamespace);
        Syntaxes.init(interp_forCurrentNamespace, env_forCurrentNamespace);
        interp_forCurrentNamespace.interp(Reader.read(resource("/core/boot0.ss")), env_forCurrentNamespace, v -> { });
    }

    static void initCurrentNamespace() {
        // Register core forms:
        // expand-expr
        Core.addCoreForm(sym("lambda"), CoreForms::lambda);
        Core.addCoreForm(sym("λ"), CoreForms::lambda);
        Core.addCoreForm(sym("case-lambda"), CoreForms::caseLambda);

        Core.addCoreForm(sym("let-values"), CoreForms.makeLetValuesForm(false, false));
        Core.addCoreForm(sym("letrec-values"), CoreForms.makeLetValuesForm(false, true));
        Core.addCoreForm(sym("letrec-syntaxes+values"), CoreForms.makeLetValuesForm(true, true));

        Core.addCoreForm(sym("#%datum"), CoreForms::datum);
        Core.addCoreForm(sym("#%app"), CoreForms::app);
        Core.addCoreForm(sym("#%top"), CoreForms::top);
        Core.addCoreForm(sym("quote"), CoreForms::quote);
        Core.addCoreForm(sym("quasiquote"), CoreForms::quasiquote);
        Core.addCoreForm(sym("quote-syntax"), CoreForms::quoteSyntax);
        Core.addCoreForm(sym("if"), CoreForms::iff);
        Core.addCoreForm(sym("with-continuation-mark"), CoreForms::withContinuationMark);
        Core.addCoreForm(sym("begin"), CoreForms::begin);
        Core.addCoreForm(sym("begin0"), CoreForms::begin);
        Core.addCoreForm(sym("set!"), CoreForms::set);

        Core.addCoreForm(sym("new"), CoreForms::new1);
        Core.addCoreForm(sym("."), CoreForms::dot);

        Core.addCoreForm(sym("provide"), CoreForms::provide);

        // expand-top-level
        Core.addCoreForm(sym("define-values"), TopLevelForms::defineValues);
        Core.addCoreForm(sym("define-syntaxes"), TopLevelForms::defineSyntaxes);


        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Core Primitives
        // for run time
        // Register core primitives:
        // This list will need to be a lot longer...

        Core.addCorePrimitive(sym("syntax-e"),
                Procedure.nameOf("syntax-e", CorePrimitives::syntax_e));
        Core.addCorePrimitive(sym("datum->syntax"),
                Procedure.nameOf("datum->syntax", CorePrimitives::datum_to_syntax));
        Core.addCorePrimitive(sym("syntax->datum"),
                Procedure.nameOf("syntax->datum", CorePrimitives::syntax_to_datum));
        Core.addCorePrimitive(sym("namespace-set-variable-value!"),
                Procedure.nameOf("namespace-set-variable-value!",
                        CorePrimitives::namespace_set_variable_value));

        env_forCurrentNamespace.env.forEach((sym, v) -> {
            if (v.value instanceof Callable) {
                Core.addCorePrimitive(sym(sym), Callable.nameOf(sym, ((Callable) v.value)));
            }
        });

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Fill in the (only) namespace, which ties the loop
        // between binding the expander
        Core.declareCoreTopLevel(Namespace.currentNamespace());
    }

    static void initExpandEnv() {
        Procedures.init(interp_forExpand, env_forExpand);
        Syntaxes.init(interp_forExpand, env_forExpand);
        Namespace.currentNamespace().variables.forEach((k, v) -> env_forExpand.put(k.name, v));
//        env_forExpand.put("current-namespace", );
//        env_forExpand.put("namespace-get-variable", );
//        env_forExpand.put("syntax-shift-phase-level", );
    }

    static void initRunTimeEnv() {
        Procedures.init(interp_forRun, env_forRun);
        Syntaxes.init(interp_forRun, env_forRun);
        Namespace.currentNamespace().variables.forEach((k, v) -> env_forRun.put(k.name, v));
//        env_forRun.put("current-namespace", );
//        env_forRun.put("namespace-get-variable", );
//        env_forRun.put("syntax-shift-phase-level", );
    }

    static void require(String path) {
        Object form = Reader.read(resource(path));
        CompiledExpression compiled = compileWithSyntax(form);
        interp_forCurrentNamespace.interp(compiled.sexpr, env_forCurrentNamespace, v -> {});
    }

    // public for test
    public static CompiledExpression compileWithSyntax(Object form) {
        Object syntax = Reader.read(resource("/core/syntax.ss"));
        Namespace ns = Namespace.currentNamespace();
        return compile(expand(list(
                sym("let-values"),
                list(),
                syntax,
                form
        ), ns), ns);
    }
}
