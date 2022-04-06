package xiao.lang;

import xiao.lang.expander.Syntax;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Symbol;

/**
 * @author chuxiaofeng
 */
public interface Pattern {

    interface Finder {
        <T> T get(String name);
    }

    Finder match(Object s);

    default @Nullable Finder tryMatch(Object s) {
        try {
            return match(s);
        } catch (InterpError e) {
            return null;
        }
    }

    // 这里用 ofStr 因为 of("s") string 本身可以做 pattern, 所以不能用重载
    static Pattern ofStr(String pattern) {
        return ofStr(pattern, new String[0]);
    }

    static Pattern ofStr(String pattern, String[] formals) {
        return of(Reader.read(pattern), formals);
    }

    static Pattern of(Object patternForm) {
        return of(patternForm, new String[0]);
    }

    static Pattern of(Object patternForm, String[] formals) {
        return s -> Matcher.wrapFinder(new Matcher(s, patternForm, formals).doMatch());
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    // todo 添加 _ 匹配任意 form
    // <pattern> = <id>      ; matches anything
    //           | id:<id>   ; matches only identifiers
    //           | (<pattern> ...)  ; zero or more
    //           | (<pattern> ...+) ; one or more
    //           | (<pattern> . <pattern>)
    class Matcher {

        // final static Predicate<String> idPredicate = s -> s.startsWith("id:");
        final static Predicate<String> idPredicate = java.util.regex.Pattern.compile("^id(:|$)").asPredicate();

        final List<Symbol> formals; // literal-ids, (syntax-rules (literal-ids) ...)
        final Object origStx;
        final Object patternForm;

        Matcher(Object origStx, Object patternForm, String[] formals) {
            this.origStx = origStx;
            this.patternForm = patternForm;
            this.formals = Arrays.stream(formals).map(RT::sym).collect(Collectors.toList());
        }

        PList doMatch() {
            return doMatch(origStx, patternForm);
        }

        PList doMatch(Object s, Object pattern) {
            if (pattern instanceof Symbol) {
                Symbol sym = (Symbol) pattern;
                if (idPredicate.test(sym.name)) {
                    expect(isIdentifier(s), "not an identifier: " + s);
                }
                if (formals.contains(sym)) {
                    expect(sym.equals(Syntax.toDatum(s)), "bad syntax: " + origStx);
                    return RT.Null();
                } else {
                    return RT.list(bingo(sym, s));
                }
            } else if (s instanceof Syntax) {
                // s instanceof Syntax 不能放第一个 if 分支是因为
                // pattern instanceof Symbol 要匹配到 syntax 对象
                // 除了 symbol 对应的syntax 不能解开, 要直接匹配
                // 其他 syntax 必须解开才能匹配
                return doMatch(((Syntax) s).e, pattern);
            } else if (isEllipsis(pattern)) {
                return doMatchEllipsis(((PList) pattern), s);
            } else if (isQuote(pattern)) {
                // 特殊处理 quote
                PList flatS = toSyntaxList(s);
                expect(isQuote(flatS) &&
                                Objects.equals(
                                        ((PList) pattern).get(1),
                                        Syntax.toDatum(flatS.get(1))
                                ),
                        "bad syntax: " + origStx);
                return RT.Null();
            } else if (RT.isPair(pattern)) { // loop pair & list
                expect(RT.isPair(s), "bad syntax: " + origStx);
                PList car = doMatch(RT.car(s), RT.car(pattern));
                PList cdr = doMatch(RT.cdr(s), RT.cdr(pattern));
                return RT.append(car, cdr); // 汇总匹配结果
            } else if (RT.isNull(pattern)) { // loop list end
                expect(RT.isNull(s), "bad syntax: " + origStx);
                return RT.Null();
            } else if (isLiteral(pattern) && s.equals(pattern)) {
                // s.equals 为啥不放在 if 里头
                // 这里要报错 bad pattern 还是 报错 bad syntax
                return RT.Null();
            } else {
                throw new InterpError("bad pattern: " + pattern);
            }
        }

        PList doMatchEllipsis(PList pattern, Object s) {
            Object repeatedPtn = pattern.get(0);
            Symbol ellipsis = ((Symbol) pattern.get(1));

            PList flatS = toSyntaxList(s);
            if (RT.isNull(flatS)) {
                boolean zeroMore = RT.sym(Names.ELLIPSIS).equals(ellipsis);
                expect(zeroMore, "bad syntax: " + origStx);
                return makeEmptyVars(pattern);
            } else {
                // todo: 想一下为啥 repeatedPattern 匹配完的结构完全一样
                //noinspection SuspiciousToArrayCall
                PList[] varArgs = RT.map(el -> doMatch(el, repeatedPtn), flatS).toArray(new PList[0]);
                return RT.map(slice -> {
                    Symbol id = ((Symbol) RT.car(RT.car(slice)));
                    PList matched = RT.map(it -> RT.car(RT.cdr(it)), slice);
                    return bingo(id, matched);
                }, varArgs);
            }
        }

        // 构造单个匹配结果
        // type matched = syntax |  list<matched>
        static PList bingo(Symbol id, Object matched) {
            return RT.list(id, matched);
        }

        PList makeEmptyVars(Object pattern) {
            if (pattern instanceof Symbol) {
                return RT.list(bingo(((Symbol) pattern), RT.Null()/*...*/));
            } else if (isEllipsis(pattern)) {
                Object repeatedPattern = RT.car(pattern);
                return makeEmptyVars(repeatedPattern);
            } else if (RT.isPair(pattern)) {
                PList car = makeEmptyVars(RT.car(pattern));
                PList cdr = makeEmptyVars(RT.cdr(pattern));
                return RT.append(car, cdr);
            } else {
                return RT.Null();
            }
        }

        // for matchEllipsis
        // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
        // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
        // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
        PList toSyntaxList(Object s) {
            if (RT.isList(s)) {
                return ((PList) s);
            } else if (RT.isPair(s)) {
                return RT.cons(RT.car(s), toSyntaxList(RT.cdr(s))); // loop
            } else if (s instanceof Syntax) {
                return toSyntaxList(((Syntax) s).e);
            } else {
                throw new InterpError("bad syntax: " + origStx);
            }
        }

        private boolean isQuote(Object pattern) {
            Symbol quote = RT.sym(Names.QUOTE);
            return  formals.contains(quote)
                    && pattern instanceof PList
                    && ((PList) pattern).size() == 2
                    && quote.equals(Syntax.toDatum(((PList) pattern).get(0)));
        }

        static boolean isIdentifier(Object pattern) {
            return pattern instanceof Symbol
                    || Syntax.isIdentifier(pattern);
        }

        // 这里可以扩展支持其他字面量匹配
        static boolean isLiteral(Object pattern) {
            return RT.isKeyword(pattern)
                    || pattern instanceof Boolean
                    || pattern instanceof Number
                    || pattern instanceof String;
        }

        boolean isEllipsis(Object pattern) {
            if (pattern instanceof PList) {
                PList lst = (PList) pattern;
                if (lst.size() == 2) {
                    Object cadr = lst.get(1);
                    return RT.sym(Names.ELLIPSIS).equals(cadr)
                            || RT.sym(Names.ELLIPSIS_PLUS).equals(cadr);
                }
            }
            return false;
        }

        static Finder wrapFinder(PList lst) {
            return new Finder() {
                @Override
                public <T> T get(String name) {
                    for (Object it : lst) {
                        PList pair = (PList) it;
                        if (((Symbol) pair.get(0)).name.equals(name)) {
                            //noinspection unchecked
                            return ((T) pair.get(1));
                        }
                    }
                    throw new InterpError("没有 pattern 变量: " + name);
                }
            };
        }
    }
}