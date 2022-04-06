package xiao.lang.expander;

import xiao.lang.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import static xiao.lang.Misc.*;
import static xiao.lang.RT.list;
import static xiao.lang.RT.sym;
import static xiao.lang.Values.PList;


/**
 * 健康宏展开 (根据马晓的 Youtube 演讲改写)
 * 演讲视频 https://www.youtube.com/watch?v=Or_yKiI3Ha4
 * 演讲ppt https://my.eng.utah.edu/~cs3520/f19/lecture27.pdf
 * racket代码 https://github.com/mflatt/expander
 * racket 文档: https://docs.racket-lang.org/reference/syntax-model.html
 * Fear of Macros: https://www.greghendershott.com/fear-of-macros/index.html
 * Writing syntax-case Macros: https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
 *
 * @author chuxiaofeng
 */
public class Expander {
    // true: compileIdentifier 把 topLevelBinding 都直接替换成 symbol
    // false: compileIdentifier 把 topLevelBinding 保留 value, 省去 interp 查找过程, but 无法 (set! $primitive ...)
    public final static boolean COMPILE_CORE_TO_SYMBOL = true;

    final Env primitiveSyntaxesEnv = Interp.syntaxes().immutable();

    /*final*/ Namespace currentNamespace;

    // COMPILE_CORE_TO_SYMBOL = true
    //      currentNamespace.toEnv(Interp.syntaxes())
    // COMPILE_CORE_TO_SYMBOL = false
    //      expand-time-namespace 展开环境只需要基础语法, 无需主动添加 procedure
    //      因为 expand+compile 使用 ctx 中的 ns, declareCoreTopLevel 已经把 procedure 加到 ns 中了
    final Env expandTImeEnv;

    // COMPILE_CORE_TO_SYMBOL = true
    //      currentNamespace.toEnv(Interp.syntaxes())
    // COMPILE_CORE_TO_SYMBOL = false
    //      run-time-namespace 运行时环境只需要基础语法, 无需主动添加 procedure
    //      因为 compileIdentifier 把 topLevelBinding 都直接替换成 ns 中的值了(procedure)
    final Env runTimeEnv;
    boolean booted;

    final static PList syntax_ss = ((PList) Reader.read(resource("/syntax.ss")));
    final static PList struct_ss = ((PList) Reader.read(resource("/struct.ss")));
    final static PList trace_ss = ((PList) Reader.read(resource("/trace.ss")));
    final static PList unit_ss = ((PList) Reader.read(resource("/unit.ss")));
    final static PList assert_ss = ((PList) Reader.read(resource("/assert.ss")));

    // 单例的用法, 不能使用 provide, 且如果 set! 会互相影响, but 现在太慢了
    private final static Expander ins = new Expander();
    public static Expander of() {
        // return new Expander();
        return ins;
    }

    private Expander() {
        currentNamespace = baseNamespace(this);

        // COMPILE_CORE_TO_SYMBOL = true 时, require 需要 syntax.ss,
        // importSyntaxes 需要 expandTime 来 eval transformer, 所以提前初始化
        expandTImeEnv = baseEnv();

        // 这里分阶段, 每阶段定义的 procedure 下一阶段的宏可用, baseEnv() 会重新从 ns 加载符号
        // hack:: provide 一些基础 procedure 到 currentNamespace
        // 在 expand 的 provide 和 CorePrimitives 的 NamespaceSetVariableValue 中实现
        require("/core0.ss", baseEnv());
        require("/core.ss", baseEnv());

        runTimeEnv = baseEnv();

        booted = true;
    }

    Namespace baseNamespace(Expander expander) {
        Namespace ns = Namespace.makeEmptyNamespace();
        Core core = initCore(expander);
        core.declareCoreTopLevel(ns);
        return ns;
    }

    Env baseEnv() {
        if (Expander.COMPILE_CORE_TO_SYMBOL) {
            return currentNamespace.toEnv(Interp.syntaxes()); // .immutable();
        } else {
            return primitiveSyntaxesEnv;
        }
    }

    public Namespace currentNamespace() {
        return currentNamespace;
    }

    public Syntax expandModule(Object form, Namespace ns) {
        // PList letValues = list(sym("let-values"), list(), form);
        Object empty = Syntaxes.beginOf(new ArrayList<>());
        PList letValues = list(sym("let-values"), list(),
                // 把初始的宏分成积分,
                // 用一个子集 (core 用到的 syntax) syntax.ss 来运行 core0.ss 和 core.ss
                // trace 会用到 core 中的函数, 所以 booted 之后再拼接
                syntax_ss,
                booted ? struct_ss : empty,
                booted ? trace_ss : empty,
                booted ? unit_ss : empty,
                booted ? assert_ss : empty,

                list(sym("debugger")), // for debugger

                // 注意!!!, 这里加个 let-values,  保持 syntax 的 scope 干净
                // 防止 syntax 引用的 primitive syntax 比如 if、lev-values (参见xiao.lang.Syntaxes)
                // 被覆盖, 导致不健康的语义
                // e.g. 如果不套这个 let-values
                // (define if (lambda (a b c) "HELLO"))
                // (and 1 2) 返回 "HELLO"
                // 套这层 let-values (and 1 2) 才返回 2
                list(sym("let-values"), list(), form)
        );
        return expand(letValues, ns);
    }

    public CompiledExpression compileModule(Object form, Namespace ns) {
        return compile(expandModule(form, ns), ns);
    }

    Object expandTimeEval(Object compiled) {
        return eval1(compiled, expandTImeEnv.derive());
    }

    Object runTimeEval(Object compiled) {
        return eval1(compiled, runTimeEnv.derive());
    }

    static Object eval1(Object s, Env env) {
        Object[] ref = new Object[1];
        Interp.interp(Reader.read(s), env, v -> ref[0] = v);
        return ref[0];
    }

    void require(String path, Env evalEnv) {
        CompiledExpression compiled = compile(pathOfRes(path));
        Interp.interp(compiled.sexpr, evalEnv, v -> {});
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    // step: read -> expand -> compile -> eval

    // import core bindings
    // The `namespace-syntax-introduce` function adds the core scope to a
    // syntax object; it needs to be used, for example, on a just-created
    // syntax object to make `lambda` refer to the core lambda form
    public static Syntax namespaceSyntaxIntroduce(Object s) {
        // All top-level bindings are in the core scope:
        // ~~The only initial bindings are in the core scope~~
        return (Syntax) Scope.add(s, Core.coreScope);
    }

    // expand-expression
    public Syntax expand(String form, Namespace ns) {
        return expand(Reader.read(form), ns);
    }

    // expand-expression
    public Syntax expand(Object form, Namespace ns) {
        // convert to syntax-object
        Object syntax = Syntax.fromDatum(null, form);
        // add core-scope
        Syntax introSyntax = namespaceSyntaxIntroduce(syntax);
        return expand(introSyntax, ns);
    }

    public Syntax expand(Syntax s, Namespace ns) {
        ExpandContext ctx = ExpandContext.makeExpandContext(this, ns);
        return Expansion.expand(s, ctx);
    }

    public CompiledExpression compile(Syntax s, Namespace ns) {
        Object compiled = Compiler.compile(s, ns);
        return new CompiledExpression(this, compiled);
    }

    public Object eval(CompiledExpression form) {
        return runTimeEval(form.sexpr);
    }

    // read+expand+compile+eval-expression
    public Object readExpandCompileEval(String s, Namespace ns) {
        Syntax expanded = expand(Reader.read(s), ns);
        CompiledExpression compiled = compile(expanded, ns);
        return eval(compiled);
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    public CompiledExpression compile(Path path) {
        String src = read(path);
        String md5 = md5(src);
        String compiledPath = path.toUri().getPath() + "." + md5;
        CompiledExpression compiled;
        if (Files.exists(Paths.get(compiledPath))) {
            Object sexpr = unSerialize(compiledPath);
            compiled = new CompiledExpression(this, sexpr);
        } else {
            Object form = Reader.read(src);
            compiled = compileModule(form, currentNamespace);
            serialize(compiledPath, compiled.sexpr);
        }
        return compiled;
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    // init core form & primitive
    static Core initCore(Expander expander) {
        Core core = new Core();

        // Register core forms

        core.addCoreForm(sym("debugger"), (s, ctx) -> {
            return Syntax.fromDatum(s, list(sym("quote-syntax"), s));
        });

        // expand-expr
        core.addCoreForm(sym("lambda"), CoreForms::lambda);
        core.addCoreForm(sym("λ"), CoreForms::lambda);
        core.addCoreForm(sym("case-lambda"), CoreForms::caseLambda);

        core.addCoreForm(sym("let-values"), CoreForms.makeLetValuesForm(false, false));
        core.addCoreForm(sym("letrec-values"), CoreForms.makeLetValuesForm(false, true));
        core.addCoreForm(sym("letrec-syntaxes+values"), CoreForms.makeLetValuesForm(true, true));

        core.addCoreForm(sym("#%datum"), CoreForms::datum);
        core.addCoreForm(sym("#%app"), CoreForms::app);
        core.addCoreForm(sym("#%top"), CoreForms::top);
        core.addCoreForm(sym("quote"), CoreForms::quote);
        core.addCoreForm(sym("quasiquote"), CoreForms::quasiquote);
        core.addCoreForm(sym("quote-syntax"), CoreForms::quoteSyntax);
        core.addCoreForm(sym("syntax"), CoreForms::syntax);
        core.addCoreForm(sym("if"), CoreForms::iff);
        core.addCoreForm(sym("with-continuation-mark"), CoreForms::withContinuationMark);
        core.addCoreForm(sym("begin"), CoreForms::begin);
        core.addCoreForm(sym("begin0"), CoreForms::begin);
        core.addCoreForm(sym("set!"), CoreForms::set);

        core.addCoreForm(sym("new"), CoreForms::new1);
        core.addCoreForm(sym("."), CoreForms::dot);

        core.addCoreForm(sym("provide"), CoreForms::provide);

        // expand-top-level
        core.addCoreForm(sym("define-values"), TopLevelForms::defineValues);
        core.addCoreForm(sym("define-syntaxes"), TopLevelForms::defineSyntaxes);


        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Register core primitives

        // 包括
        // 1. interp procedure
        // 2. core.ss 中加入环境的 procedure
        // 3. syntax-e datum->syntax syntax->datum namespace-set-variable-value! ...
        Interp.procedures(expander).forEach((sym, v) -> {
            core.addCorePrimitive(sym(sym), (Values.Procedure) v);
        });

        // expand time 用到的 procedure

        // syntax
        addCorePrimitive(core, "syntax?", CorePrimitives::isSyntax);
        addCorePrimitive(core, "identifier?", CorePrimitives::isIdentifier);
        addCorePrimitive(core, "syntax-e", CorePrimitives::syntax_e);
        // todo (syntax-property stx prop)
        addCorePrimitive(core, "datum->syntax", CorePrimitives::datum_to_syntax);
        addCorePrimitive(core, "syntax->datum", CorePrimitives::syntax_to_datum);
        addCorePrimitive(core, "syntax->list", CorePrimitives::syntax_to_list);

        // binding
        addCorePrimitive(core, "free-identifier=?", CorePrimitives::isFreeIdentifierEquals);
        addCorePrimitive(core, "identifier-binding", CorePrimitives::identifierBinding);

        addCorePrimitive(core, "namespace-syntax-introduce", CorePrimitives::namespaceSyntaxIntroduce);

        // namespace
        addCorePrimitive(core, "current-namespace", new CorePrimitives.CurrentNamespace(expander));
        addCorePrimitive(core, "namespace-variable-value", new CorePrimitives.NamespaceVariableValue(expander));
        addCorePrimitive(core, "namespace-set-variable-value!", new CorePrimitives.NamespaceSetVariableValue(expander));
        return core;
    }

    static void addCorePrimitive(Core core, String sym, Values.Procedure procedure) {
        core.addCorePrimitive(sym(sym), Values.Procedure.nameOf(sym, procedure));
    }
}
