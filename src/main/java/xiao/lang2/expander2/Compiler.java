package xiao.lang2.expander2;

import xiao.lang2.InterpError;
import xiao.lang2.Procedures;

import java.util.List;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Pattern.Finder;
import static xiao.lang2.Procedures.*;
import static xiao.lang2.Reader.MemberAccessorExpansion.isJavaClassName;
import static xiao.lang2.Values.PList;
import static xiao.lang2.Values.Symbol;
import static xiao.lang2.expander2.Bindings.LocalBinding;
import static xiao.lang2.expander2.Bindings.TopLevelBinding;
import static xiao.lang2.expander2.CUtils.map;
import static xiao.lang2.expander2.Utils.match;
import static xiao.lang2.expander2.Utils.tryMatch;

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
    public static Object compile(Syntax s) {
        // compile-time namespace
        return compile(s, Namespace.currentNamespace());
    }

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
            Symbol sym = b.symbol();
            Object corePrimitive = ns.getVariable(sym, null);
            expect(corePrimitive != null, "missing a top-binding after expansion: " + b);
            return corePrimitive;
        } else {
            throw new InterpError("not a reference to a local binding: " + s);
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
                return list(
                        coreSym,
                        compileLambdaFormals(formals),
                        compile(body, ns)
                );
            }
            case "case-lambda": {
                Finder r = match("(case-lambda [formals body] ...)", s);
                List<Syntax> formalss = r.get("formals");
                List<Syntax> bodys = r.get("body");
                return list(
                        coreSym,
                        splice(map(formalss, bodys, (formals, body) -> list(
                                compileLambdaFormals(formals),
                                compile(body, ns)
                        )))
                );
            }
            case "#%app": {
                Finder r = match("(#%app . rest)", s);
                List<Syntax> rest = r.get("rest");
                return listColl(map(rest, it -> compile(it, ns)));
            }
            case "if": {
                Finder r = match("(if tst thn els)", s);
                Syntax tst = r.get("tst");
                Syntax thn = r.get("thn");
                Syntax els = r.get("els");
                return list(
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
                return list(
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
                return list(
                        coreSym,
                        splice(map(es, e -> compile(e, ns)))
                );
            }
            case "set!": {
                return compileSet(coreSym, s, ns);
            }
            case "let-values":
            case "letrec-values":
                return compileLet(coreSym, s, ns);
            case "quote": {
                Finder r = match("(quote datum)", s);
                Syntax datum = r.get("datum");
                return list(
                        coreSym,
                        // strip scopes for quote
                        Syntax.toDatum(datum)
                );
            }
            case "quasiquote": {
                Finder r = match("(quasiquote datum)", s);
                Syntax datum = r.get("datum");
                expect(Syntax.isList(datum));
                return list(
                        coreSym,
                        compileQuasiquote(datum)
                );
            }
            case "quote-syntax": {
                Finder r = match("(quote-syntax datum)", s);
                Syntax datum = r.get("datum");
                return list(
                        sym("quote"),
                        // preserve scopes for quote-syntax
                        datum
                );
            }
            case "new": {
                Finder r = match("(new id:class-name args ...)", s);
                Syntax className = r.get("id:class-name");
                List<Syntax> args = r.get("args");
                return list(
                        sym("new"),
                        Syntax.toDatum(className),
                        splice(map(args, arg -> compile(arg, ns)))
                );
            }
            case ".": {
                return compileDot(coreSym, s, ns);
            }
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
        return list(
                coreSym,
                compile(id, ns),
                compile(rhs, ns)
        );
    }

    static Object compileSetField(Symbol coreSym, Syntax s, Namespace ns) {
        Finder r = match("(set! (. cls-or-ins id:field) rhs)", new String[] { "." }, s);
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax field = r.get("id:field");
        Syntax rhs = r.get("rhs");

        // (set! (. className static-field-name) expr)
        // (set! (. instance-expr instance-field-name) expr)
        return list(
                coreSym,
                list(
                        sym("."),
                        compileClassOrInstance(clsOrIns),
                        Syntax.toDatum(field)
                ),
                compile(rhs, ns)
        );
    }

    static Object compileQuasiquote(Object datum) {
        Object lst = CoreForms.toSyntaxList(datum);
        if (lst == Boolean.FALSE) {
            return Syntax.toDatum((datum));
        } else if (isNull(lst)) {
            return Syntax.toDatum((datum));
        } else if (isPair(lst)) {
            Object rest = compileQuasiquote(cdr(lst));
            Finder r = tryMatch("(id:unquote form)", car(lst));
            if (r != null) {
                Syntax unquote = r.get("id:unquote");
                Syntax form = r.get("form");
                String name = ((Symbol) unquote.e).name;
                if (name.equals("unquote") || name.equals("unquote-splicing")) {
                    return cons(list(sym(name), compile(form)), rest);
                } else {
                    Object fst = compileQuasiquote(car(lst));
                    return cons(Syntax.toDatum(fst), rest);
                }
            } else {
                Object fst = compileQuasiquote(car(lst));
                return cons(Syntax.toDatum(fst), rest);
            }
        } else {
            return Syntax.toDatum((datum));
        }
    }

    // (case-lambda (formals body ...+) ...)
    // formals = (id ...)
    // 	 	|	 	(id ...+ . rest-id)
    // 	 	|	 	rest-id
    static Object compileLambdaFormals(Object formals) {
        if (Syntax.isIdentifier(formals)) {
            if ((Syntax.isDot(formals))) {
                return ((Syntax) formals).e;
            } else {
                return local2symbol(((Syntax) formals));
            }
        } else if (formals instanceof Syntax) {
            return compileLambdaFormals(((Syntax) formals).e);
        } else if (formals instanceof PList) {  // fast-route
            return Procedures.map(Compiler::compileLambdaFormals, (PList) formals);
        } else if (isPair(formals)) {
            return cons(
                    compileLambdaFormals(car(formals)),
                    compileLambdaFormals(cdr(formals))
            );
        } else {
            return Procedures.Null();
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
        return list(
                coreSym,
                listColl(map(idss, rhss, (ids, rhs) -> list(
                        listColl(map(ids, Compiler::local2symbol)),
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
        // 实例静态方法调用
        // (. instance-expr (method-symbol args ...))
        // (. ClassName-symbol (method-symbol args ...))
        Finder r = match("(. cls-or-ins (id:method-name args ...))", s);
        Syntax clsOrIns = r.get("cls-or-ins");
        Syntax methodName = r.get("id:method-name");
        List<Syntax> args = r.get("args");

        return list(
                coreSym,
                compileClassOrInstance(clsOrIns),
                list(
                        Syntax.toDatum(methodName),
                        splice(map(args, arg -> compile(arg, ns)))
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

        return list(
                coreSym,
                compileClassOrInstance(clsOrIns),
                Syntax.toDatum(member),
                splice(map(args, arg -> compile(arg, ns)))
        );
    }

    static Object compileClassOrInstance(Syntax clsOrIns) {
        return isJavaClassName(clsOrIns.unbox()) ? Syntax.toDatum(clsOrIns) : compile(clsOrIns);
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