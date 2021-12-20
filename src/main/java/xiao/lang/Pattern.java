package xiao.lang;

import xiao.lang.expander.Syntax;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Procedures.*;
import static xiao.lang.Values.*;

/**
 * 实现了两种模式匹配
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
        return of(Patterns.make(pattern), formals);
    }

    static Pattern of(Object patternForm) {
        return of(patternForm, new String[0]);
    }

    static Pattern of(Object patternForm, String[] formals) {
        // return of1(patternForm, formals);
        return of2(patternForm, formals);
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class Patterns {
        final static Interp interp;
        final static Env env;
        static {
            interp = new Interp(false);
            env = new Env();
            Procedures.init(interp, env);
            Syntaxes.init(interp, env);
        }
        // 要支持 dot nation, 所以改成 eval
        static Object make(String pattern) {
            Object[] ref = new Object[1];
            Object patternForm = Parser.parse1(Parser.QUOTE + pattern);
            interp.interp(patternForm, env.derive(), ret -> ref[0] = ret);
            return ref[0];
        }
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    static Pattern of1(Object patternForm) {
        return of1(patternForm, new String[0]);
    }

    static Pattern of1(Object patternForm, String[] formals) {
        return s -> new SyntaxRule(patternForm, formals).match(s);
    }

    static Pattern of2(Object patternForm) {
        return of2(patternForm, new String[0]);
    }

    static Pattern of2(Object patternForm, String[] formals) {
        return s -> Matcher.wrapFinder(new Matcher(s, patternForm, formals).doMatch());
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

    class SyntaxRule {
        // final static Predicate<String> idPredicate = s -> s.startsWith("id:");
        final static Predicate<String> idPredicate = java.util.regex.Pattern.compile("^id(:|$)").asPredicate();

        final static List<Symbol> reserved = new ArrayList<>();
        static {
            reserved.add(sym(Names.ELLIPSIS));
            reserved.add(sym(Names.ELLIPSIS_PLUS));
            reserved.add(sym(Names.UNDERSCORE));
        }

        final List<Symbol> formals; // literal-ids, (syntax-rules (literal-ids) ...)
        final Object pattern;

        public SyntaxRule(Object patternForm, String[] formals) {
            this.formals = Arrays.stream(formals).map(Procedures::sym).collect(Collectors.toList());
            this.pattern = patternForm;
        }

        public Finder match(Object inputForm) {
            Matches matches = new Matches(pattern, formals);
            boolean matched = match(pattern, inputForm, matches, 0);
            if (matched) {
                return matches;
            } else {
                throw new InterpError("匹配失败: " + pattern + " <=> " + inputForm);
            }
        }

        // binding 用来处理模式匹配的绑定的返回值,  Symbol => Object|List<Object>
        // 这里是 cells 是 List<Object>, 包装成 PList
//        public boolean match(Object input, Env binding) {
//            Matches matches = new Matches(pattern, formals);
//            boolean matched = match(pattern, input, matches, 0);
//            if (matched) {
//                matches.binding.forEach((k, v) -> {
//                    binding.put(k.name, matches.get(k));
//                });
//            }
//            return matched;
//        }

        @Override
        public String toString() {
            return pattern.toString();
        }

        boolean match(Object pattern, Object input, Matches matches, int depth) {
            if (pattern instanceof PList) {
                Object flatS = toSyntaxList(input); // 特殊处理 syntax 匹配
                if (flatS instanceof PList) {
                    ListInput listInput = new ListInput(((PList) flatS), matches);
                    return matchList(((PList) pattern), listInput, depth);
                } else {
                    return false;
                }
            } else if (pattern instanceof Pair) {
                if (input instanceof Pair) {
                    return matchPair(((Pair) pattern), ((Pair) input), matches, depth);
                } else {
                    return false;
                }
            } else if (pattern instanceof Symbol) {
                return matchSymbol((Symbol) pattern, input, matches);
            } else {
                return pattern.equals(input);
            }
        }

        // return PList | false
        // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
        // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
        // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
        public static Object toSyntaxList(Object s) {
            if (isList(s)) {
                return s;
            } else if (isPair(s)) {
                Object cdr = toSyntaxList(cdr(s));
                if (cdr == Boolean.FALSE) {
                    return cdr;
                } else {
                    return cons(car(s), cdr); // loop
                }
            } else if (s instanceof Syntax) {
                return toSyntaxList(((Syntax) s).e);
            } else {
                return false;
            }
        }

        class ListInput {
            final PList input;
            final Matches matches;
            private int idx = 0;

            ListInput(PList input, Matches matches) {
                this.input = input;
                this.matches = matches;
            }

            boolean match(Object pattern, int depth) {
                if (hasNext()) {
                    Object input = next();
                    boolean matched = SyntaxRule.this.match(pattern, input, matches, depth);
                    if (matched) {
                        idx++;
                    }
                    return matched;
                } else {
                    return false;
                }
            }

            Object next() {
                return input.get(idx);
            }

            boolean hasNext() {
                return idx < input.size();
            }
        }

        boolean matchList(PList pattern, ListInput listInput, int depth) {
            boolean empty = pattern.isEmpty();
            if (empty) {
                return pattern.equals(listInput.input);
            }

            // 特殊处理 quote
            if (isQuote(pattern) && isQuote(listInput.input)) {
                return Objects.equals(pattern.get(1), listInput.input.get(1));
            }

            for (int i = 0; i < pattern.size(); i++) {
                Object curPatEl = pattern.get(i);
                Object nxtPatEl = i + 1 < pattern.size() ? pattern.get(i + 1) : null;

                if (sym(Names.ELLIPSIS).equals(curPatEl) || sym(Names.ELLIPSIS_PLUS).equals(curPatEl)) {
                    continue;
                }

                boolean followedByEllipsis = nxtPatEl != null && (
                        sym(Names.ELLIPSIS).equals(nxtPatEl) || sym(Names.ELLIPSIS_PLUS).equals(nxtPatEl)
                );
                boolean zeroMore = followedByEllipsis && sym(Names.ELLIPSIS).equals(nxtPatEl);

                depth = depth + (followedByEllipsis ? 1 : 0);

                // 遇到重复, pattern 中的变量的树加一层，并且把树层级指针移到该层
                // 匹配到的 子树或者 node 都存储到新加的一层
                // 用子树表达一组重复的值
                if (followedByEllipsis) {
                    listInput.matches.descend(patternVars(curPatEl, formals), depth);
                }

                boolean consumed = listInput.match(curPatEl, depth);

                if (consumed) {
                    if (followedByEllipsis) {
                        //noinspection StatementWithEmptyBody
                        while (listInput.match(curPatEl, depth)) { }
                    }
                } else {
                    if (!zeroMore) {
                        return false;
                    }
                }
            }

            return !listInput.hasNext();
        }

        boolean matchPair(Pair pattern, Pair input, Matches matches, int depth) {
            return match(pattern.car, input.car, matches, depth)
                    && match(pattern.cdr, input.cdr, matches, depth);
        }

        boolean matchSymbol(Symbol sym, Object input, Matches matches) {
            if (idPredicate.test(sym.name)) {
                expect(isIdentifier(input), "not an identifier: " + input);
            }

            if (formals.contains(sym)) {
                return sym.equals(input);
            } else {
                matches.put(sym, input);
                return true;
            }
        }

        static class TreeNode {
            List<Object/*TreeNode|Node*/> nodes = new ArrayList<>();

            void add(TreeNode node) { nodes.add(node); }

            void add(Object node) { nodes.add(node); }

            TreeNode last() {
                expect(!nodes.isEmpty(), "!nodes.isEmpty()");
                Object last = nodes.get(nodes.size() - 1);
                expect(last instanceof TreeNode, "last instance TreeNode");
                return ((TreeNode) last);
            }
        }

        // Ast.Node 或者说 form 只保存在叶子节点
        static class Tree {
            final String name;
            private int depth = 0; // 只增不减
            TreeNode root = new TreeNode();

            public Tree(String name) {
                this.name = name;
            }

            // 重复深度下降
            // ([var init step ...] ...)
            // ([var init step .../*depth=1*/] .../*depth=0*/)
            // var = [a,b,...], init = [a,b,...], step = [[a,b,...], [a,b,...], ...]
            // 因为外层是递归匹配 (深度优先), 所以匹配是只 +depth 就可以
            void descend(int depth) {
                tail(depth - 1).add(new TreeNode());
                if (depth > this.depth) {
                    this.depth = depth;
                }
            }

            void add(Object value) {
                tail(depth).add(value);
            }

            private TreeNode tail(int depth) {
                TreeNode node = root;
                for (int i = 0; i < depth; i++) {
                    node = node.last();
                }
                return node;
            }
        }

        class Matches implements Finder {
            final Map<Symbol, Tree> binding = new LinkedHashMap<>();

            Matches(Object pattern, List<Symbol> formals) {
                Set<Symbol> patternVars = patternVars(pattern, formals);
                for (Symbol name : patternVars) {
                    binding.put(name, new Tree(name.name));
                }
            }

            void descend(Set<Symbol> patternVars, int depth) {
                binding.forEach((name, tree) -> {
                    if (patternVars.contains(name)) {
                        tree.descend(depth);
                    }
                });
            }

            void put(Symbol name, Object value) {
                if (binding.containsKey(name)) {
                    binding.get(name).add(value);
                }
            }

            // java 只有异常有 union type, 不好表达自己看着用吧, 不保证类型
            // type T = List<T> | Object
            // type Result = List<Result> | Object
            @Override
            public <T> T get(String name) {
                Tree tree = binding.get(sym(name));
                if (tree == null) {
                    throw new InterpError("没有找到 pattern 变量: " + name);
                } else {
                    //noinspection unchecked
                    return ((T) treeNode2Result(tree.root.nodes.get(0)));
                }
            }

            Object treeNode2Result(/*TreeNode|Object*/Object o) {
                if (o instanceof TreeNode) {
                    List<Object> ns = ((TreeNode) o).nodes;
                    List<Object> lst = new ArrayList<>(ns.size());
                    for (Object it : ns) {
                        if (it instanceof TreeNode) {
                            lst.add(treeNode2Result(it));
                        } else {
                            lst.add(it);
                        }
                    }
                    return lst;
                } else {
                    return o;
                }
            }
        }

        Set<Symbol> patternVars(Object pattern, List<Symbol> excluded) {
            Set<Symbol> result = new LinkedHashSet<>();
            patternVars1(pattern, excluded, result);
            return result;
        }

        private void patternVars1(Object pattern, List<Symbol> excluded, Set<Symbol> results) {
            if (pattern instanceof Symbol) {
                Symbol name = (Symbol) pattern;
                if (!excluded.contains(name) && !reserved.contains(name)) {
                    results.add(name);
                }
            } else if (pattern instanceof PList) {
                // 特殊处理 quote for case-lambda
                if (isQuote(((PList) pattern))) {
                    return;
                }

                for (Object el : ((PList) pattern)) {
                    patternVars1(el, excluded, results);
                }
            }
        }

        private boolean isQuote(Object pattern) {
            Symbol quote = sym(Names.QUOTE);
            return  formals.contains(quote)
                    && pattern instanceof PList
                    && ((PList) pattern).size() == 2
                    && quote.equals(Syntax.toDatum(((PList) pattern).get(0)));
        }

        static boolean isIdentifier(Object pattern) {
            return pattern instanceof Symbol
                    || Syntax.isIdentifier(pattern);
        }
    }

    // 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

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
            this.formals = Arrays.stream(formals).map(Procedures::sym).collect(Collectors.toList());
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
                    return Null();
                } else {
                    return list(bingo(sym, s));
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
                return Null();
            } else if (isPair(pattern)) { // loop pair & list
                expect(isPair(s), "bad syntax: " + origStx);
                PList car = doMatch(car(s), car(pattern));
                PList cdr = doMatch(cdr(s), cdr(pattern));
                return append(car, cdr); // 汇总匹配结果
            } else if (isNull(pattern)) { // loop list end
                expect(isNull(s), "bad syntax: " + origStx);
                return Null();
            } else if (isLiteral(pattern) && s.equals(pattern)) {
                // s.equals 为啥不放在 if 里头
                // 这里要报错 bad pattern 还是 报错 bad syntax
                return Null();
            } else {
                throw new InterpError("bad pattern: " + pattern);
            }
        }

        PList doMatchEllipsis(PList pattern, Object s) {
            Object repeatedPtn = pattern.get(0);
            Symbol ellipsis = ((Symbol) pattern.get(1));

            PList flatS = toSyntaxList(s);
            if (isNull(flatS)) {
                boolean zeroMore = sym(Names.ELLIPSIS).equals(ellipsis);
                expect(zeroMore, "bad syntax: " + origStx);
                return makeEmptyVars(pattern);
            } else {
                // todo: 想一下为啥 repeatedPattern 匹配完的结构完全一样
                //noinspection SuspiciousToArrayCall
                PList[] varArgs = map(el -> doMatch(el, repeatedPtn), flatS).toArray(new PList[0]);
                return map(slice -> {
                    Symbol id = ((Symbol) car(car(slice)));
                    PList matched = map(it -> car(cdr(it)), slice);
                    return bingo(id, matched);
                }, varArgs);
            }
        }

        // 构造单个匹配结果
        // type matched = syntax |  list<matched>
        static PList bingo(Symbol id, Object matched) {
            return list(id, matched);
        }

        static PList makeEmptyVars(Object pattern) {
            if (pattern instanceof Symbol) {
                return list(bingo(((Symbol) pattern), Null()/*...*/));
            } else if (isEllipsis(pattern)) {
                Object repeatedPattern = car(pattern);
                return makeEmptyVars(repeatedPattern);
            } else if (isPair(pattern)) {
                PList car = makeEmptyVars(car(pattern));
                PList cdr = makeEmptyVars(cdr(pattern));
                return append(car, cdr);
            } else {
                return Null();
            }
        }

        // for matchEllipsis
        // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
        // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
        // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
        PList toSyntaxList(Object s) {
            if (isList(s)) {
                return ((PList) s);
            } else if (isPair(s)) {
                return cons(car(s), toSyntaxList(cdr(s))); // loop
            } else if (s instanceof Syntax) {
                return toSyntaxList(((Syntax) s).e);
            } else {
                throw new InterpError("bad syntax: " + origStx);
            }
        }

        private boolean isQuote(Object pattern) {
            Symbol quote = sym(Names.QUOTE);
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
            return isKeyword(pattern)
                    || pattern instanceof Boolean
                    || pattern instanceof Number
                    || pattern instanceof String;
        }

        static boolean isEllipsis(Object pattern) {
            if (pattern instanceof PList) {
                PList lst = (PList) pattern;
                if (lst.size() == 2) {
                    Object cadr = lst.get(1);
                    return sym(Names.ELLIPSIS).equals(cadr)
                            || sym(Names.ELLIPSIS_PLUS).equals(cadr);
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