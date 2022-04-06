package xiao.lang.expander;

import xiao.lang.InterpError;
import xiao.lang.RT;

import java.util.List;

import static xiao.lang.Contract.expect;
import static xiao.lang.Pattern.Finder;
import static xiao.lang.Reader.isJavaClassName;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.Binding.LocalBinding;
import static xiao.lang.expander.Binding.TopLevelBinding;
import static xiao.lang.expander.CUtils.map;
import static xiao.lang.expander.SyntaxMatcher.match;
import static xiao.lang.expander.SyntaxMatcher.tryMatch;

/**
 * 1.2.4 Compilation : bridge to the host
 * https://docs.racket-lang.org/reference/syntax-model.html
 *
 * Before expanded code is evaluated, it is first compiled.
 * A compiled form has essentially the same information as the corresponding expanded form,
 * though the internal representation naturally dispenses with identifiers for syntactic forms and local bindings.
 * One significant difference is that a compiled form is almost entirely opaque,
 * so the information that it contains cannot be accessed directly (which is why some identifiers can be dropped).
 * At the same time, a compiled form can be marshaled to and from a byte string, so it is suitable for saving and re-loading code.
 *
 * Although individual read, expand, compile, and evaluate operations are available,
 * the operations are often combined automatically.
 * For example, the eval procedure takes a syntax object and expands it, compiles it, and evaluates it.
 *
 * @author chuxiaofeng
 */
public class Compiler {

    // Convert an expanded syntax object to an expression that is represented
    // by a plain S-expression.

    public static Object compile(Syntax s, Namespace ns) {
        if (Syntax.isPair(s)) {
            return compilePair(s, ns);
        } else if (Syntax.isIdentifier(s)) {
            return compileIdentifier(s, ns);
        } else {
            throw new InterpError("bad syntax after expansion: " + s);
        }
    }

    static Object compileIdentifier(Syntax s, Namespace ns) {
        Binding b = Scope.resolve(s);
        if (b instanceof LocalBinding) {
            // A local-binding key is already a symbol
            Symbol sym = b.symbol();
            expect(sym != null, "missing a local-binding after expansion: " + b);
            return sym;
        } else if (b instanceof TopLevelBinding) {
            return compileTopLevelBinding(((TopLevelBinding) b), ns);
        } else {
            throw new InterpError("not a reference to a local binding: " + s);
        }
    }

    static Object compileTopLevelBinding(TopLevelBinding b, Namespace ns) {
        Symbol sym = b.symbol();
        Object corePrimitive = ns.getVariable(sym, null);
        expect(corePrimitive != null, "missing a top-binding after expansion: " + b);

        if (Expander.COMPILE_CORE_TO_SYMBOL) {
            // 因为局部变量都被 gensym 了, 所以可以直接替换成名字
            String s1 = corePrimitive.toString();
            if (s1.startsWith("#<procedure:")) {
                return RT.sym(s1.substring("#<procedure:".length(), s1.length() - 1));
            } else if (s1.startsWith("#<syntax:")) {
                return RT.sym(s1.substring("#<syntax:".length(), s1.length() - 1));
            } else {
                // 直接返回数据分支
                // namespace-set-variable-value! 引入的, 必须是 (quote datum) 形式
                if (corePrimitive instanceof PList) {
                    PList lst = (PList) corePrimitive;
                    if (lst.size() == 2) {
                        if (lst.get(0).equals(RT.sym("quote"))) {
                            return corePrimitive;
                        }
                    }
                }
                // 其他场景不应该发生
                throw new InterpError("不支持编译的 identifier: " + corePrimitive);
            }
        } else {
            return corePrimitive;
        }
    }

    static Object compilePair(Syntax s, Namespace ns) {
        Symbol coreSym = Core.coreFormSym(s);
        expect(coreSym != null, "not a core form: " + s);
        // accommodate non-function compile-time bindings
        switch (coreSym.name) {
            case "λ":
            case "lambda": {
                Finder r = match("(lambda formals body)", s);
                Syntax formals = r.get("formals");
                Syntax body = r.get("body");
                return RT.list(
                        coreSym,
                        compileLambdaFormals(formals),
                        compile(body, ns)
                );
            }
            case "case-lambda": {
                Finder r = match("(case-lambda [formals body] ...)", s);
                List<Syntax> formalss = r.get("formals");
                List<Syntax> bodys = r.get("body");
                return RT.list(
                        coreSym,
                        RT.splice(map(formalss, bodys, (formals, body) -> RT.list(
                                compileLambdaFormals(formals),
                                compile(body, ns)
                        )))
                );
            }
            case "#%app": {
                Finder r = match("(#%app . rest)", s);
                List<Syntax> rest = r.get("rest");
                return RT.listColl(map(rest, it -> compile(it, ns)));
            }
            case "if": {
                Finder r = match("(if tst thn els)", s);
                Syntax tst = r.get("tst");
                Syntax thn = r.get("thn");
                Syntax els = r.get("els");
                return RT.list(
                        coreSym,
                        compile(tst, ns),
                        compile(thn, ns),
                        compile(els, ns)
                );
            }
            case "with-continuation-mark": {
                Finder r = match("(with-continuation-mark key val body)", s);
                Syntax key = r.get("key");
                Syntax val = r.get("val");
                Syntax body = r.get("body");
                return RT.list(
                        coreSym,
                        compile(key, ns),
                        compile(val, ns),
                        compile(body, ns)
                );
            }
            case "begin":
            case "begin0": {
                Finder r = match("(begin e ...+)", s);
                List<Syntax> es = r.get("e");
                if (es.size() == 1) {
                    return compile(es.get(0), ns);
                } else {
                    return RT.list(
                            coreSym,
                            RT.splice(map(es, e -> compile(e, ns)))
                    );
                }
            }
            case "set!":
                return compileSet(coreSym, s, ns);
            case "let-values":
            case "letrec-values":
                return compileLet(coreSym, s, ns);
            case "quote": {
                Finder r = match("(quote datum)", s);
                Syntax datum = r.get("datum");
                return RT.list(
                        coreSym,
                        // strip scopes for quote
                        Syntax.toDatum(datum)
                );
            }
            // expand 阶段已经展开成 append list list* 形式
            // case "quasiquote": { }
            case "quote-syntax": {
                Finder r = tryMatch("(quote-syntax datum)", s);
                if (r == null) {
                    r = match("(quote-syntax datum local)", s);
                }
                Syntax datum = r.get("datum");
                return RT.list(
                        RT.sym("quote"),
                        // preserve scopes for quote-syntax
                        datum
                );
            }
            case "new": {
                Finder r = match("(new id:class-name args ...)", s);
                Syntax className = r.get("id:class-name");
                List<Syntax> args = r.get("args");
                return RT.list(
                        RT.sym("new"),
                        Syntax.toDatum(className),
                        RT.splice(map(args, arg -> compile(arg, ns)))
                );
            }
            case ".":
                return compileDot(coreSym, s, ns);
            default:
                throw new InterpError("unrecognized core form: " + coreSym);
        }
    }

    static Object compileSet(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = tryMatch("(set! id rhs)", s);
        if (r == null) {
            return compileSetField(coreSym, s, ns);
        } else {
            return compileSetId(coreSym, s, ns);
        }
    }

    static Object compileSetId(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = match("(set! id rhs)", s);
        Syntax id = r.get("id");
        Syntax rhs = r.get("rhs");
        return RT.list(
                coreSym,
                compile(id, ns),
                compile(rhs, ns)
        );
    }

    static Object compileSetField(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = match("(set! (. cls-or-ins id:field) rhs)", s, ".");
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax field = r.get("id:field");
        Syntax rhs = r.get("rhs");

        // (set! (. className static-field-name) expr)
        // (set! (. instance-expr instance-field-name) expr)
        return RT.list(
                coreSym,
                RT.list(
                        RT.sym("."),
                        compileClassOrInstance(clsOrIns, ns),
                        Syntax.toDatum(field)
                ),
                compile(rhs, ns)
        );
    }

    // (case-lambda (formals body ...+) ...)
    // formals = (id ...)
    // 	 	|	 	(id ...+ . rest-id)
    // 	 	|	 	rest-id
    static Object compileLambdaFormals(Object formals) {
        if (Syntax.isIdentifier(formals)) {
            return local2symbol(((Syntax) formals));
        } else if (formals instanceof Syntax) {
            return compileLambdaFormals(((Syntax) formals).e);
        } else if (formals instanceof PList) {
            // fast-route
            return RT.map(Compiler::compileLambdaFormals, (PList) formals);
        } else if (RT.isPair(formals)) {
            return RT.cons(
                    compileLambdaFormals(RT.car(formals)),
                    compileLambdaFormals(RT.cdr(formals))
            );
        } else {
            return RT.Null();
        }
    }

    static Object compileLet(Symbol coreSym, Syntax s, Namespace ns) {
        // boolean rec = "letrec-values".equals(coreSym.name);
        // (let[rec]-values ([(id ...) val-expr] ...) body ...+)
        Finder r = match("(_ ([(id ...) rhs] ...) body)", s);
        List<List<Syntax>> idss = r.get("id");
        List<Syntax> rhss = r.get("rhs");
        Syntax body = r.get("body");
        // Scope sc = Scope.of();
        return RT.list(
                coreSym,
                RT.listColl(map(idss, rhss, (ids, rhs) -> RT.list(
                        RT.listColl(map(ids, Compiler::local2symbol)),
                        compile(rhs, ns)
                ))),
                compile(body, ns)
        );
    }

    static Object compileDot(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = tryMatch("(. cls-or-ins (id:method-name args ...))", s);
        boolean isMethodCall = r != null;
        if (isMethodCall) {
            return compileDotMethod(coreSym, s, ns);
        } else {
            return compileDot1(coreSym, s, ns);
        }
    }

    static Object compileDotMethod(Symbol coreSym, Syntax s, Namespace ns) {
        // 注意: expansion 改写这里不准确, 属性访问也统一被改写成无参方法调用
        // (.instanceMember instance-expr) ==> (. instance-expr (instanceMember))
        // (.instanceMember ClassName) ==> (. ClassName (instanceMember))

        // 实例静态方法调用
        // (. instance-expr (method-symbol args ...))
        // (. ClassName-symbol (method-symbol args ...))
        Finder r = match("(. cls-or-ins (id:method-name args ...))", s);
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax methodName = r.get("id:method-name");
        List<Syntax> args = r.get("args");

        return RT.list(
                coreSym,
                compileClassOrInstance(clsOrIns, ns),
                RT.list(
                        Syntax.toDatum(methodName),
                        RT.splice(map(args, arg -> compile(arg, ns)))
                )
        );
    }

    // 实例静态属性访问
    // (. instance-expr member-symbol)
    // (. ClassName-symbol member-symbol)
    // (. instance-expr -field-symbol)
    // 实例静态方法调用
    // (. instance-expr method-symbol args ...)
    // (. ClassName-symbol method-symbol args ...)
    static Object compileDot1(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = match("(. cls-or-ins id:member args ...)", s);
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax member = r.get("id:member");
        List<Syntax> args = r.get("args");

        return RT.list(
                coreSym,
                compileClassOrInstance(clsOrIns, ns),
                Syntax.toDatum(member),
                RT.splice(map(args, arg -> compile(arg, ns)))
        );
    }

    static Object compileClassOrInstance(Syntax clsOrIns, Namespace ns) {
        return isJavaClassName(clsOrIns.unbox()) ? Syntax.toDatum(clsOrIns) : compile(clsOrIns, ns);
    }

    // local->symbol
    static Symbol local2symbol(Syntax id) {
        Binding b = Scope.resolve(id);
        if (b instanceof LocalBinding) {
            return b.symbol();
        } else {
            throw new InterpError("bad binding: " + id);
        }
    }
}