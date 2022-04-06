package xiao.lang.pattern;

import xiao.lang.*;
import xiao.lang.expander.Syntax;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static xiao.lang.Contract.expect;

/**
 * @author chuxiaofeng
 */
public class SyntaxRule implements Pattern {
    // final static Predicate<String> idPredicate = s -> s.startsWith("id:");
    final static Predicate<String> idPredicate = java.util.regex.Pattern.compile("^id(:|$)").asPredicate();

    final static List<Values.Symbol> reserved = new ArrayList<>();

    static {
        reserved.add(RT.sym(Names.ELLIPSIS));
        reserved.add(RT.sym(Names.ELLIPSIS_PLUS));
        reserved.add(RT.sym(Names.UNDERSCORE));
    }

    final List<Values.Symbol> formals; // literal-ids, (syntax-rules (literal-ids) ...)
    final Object pattern;

    public SyntaxRule(Object patternForm, String[] formals) {
        this.formals = Arrays.stream(formals).map(RT::sym).collect(Collectors.toList());
        this.pattern = patternForm;
    }

    public Pattern.Finder match(Object inputForm) {
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
        if (pattern instanceof Values.PList) {
            Object flatS = toSyntaxList(input); // 特殊处理 syntax 匹配
            if (flatS instanceof Values.PList) {
                ListInput listInput = new ListInput(((Values.PList) flatS), matches);
                return matchList(((Values.PList) pattern), listInput, depth);
            } else {
                return false;
            }
        } else if (pattern instanceof Values.Pair) {
            if (input instanceof Values.Pair) {
                return matchPair(((Values.Pair) pattern), ((Values.Pair) input), matches, depth);
            } else {
                return false;
            }
        } else if (pattern instanceof Values.Symbol) {
            return matchSymbol((Values.Symbol) pattern, input, matches);
        } else {
            return pattern.equals(input);
        }
    }

    // return PList | false
    // 把符合 list 形式的 syntax 解开 syntax 处理成正常的 list<syntax>, e.g.
    // (syntax:1 . syntax:(syntax:2 syntax:3)) => (syntax:1 syntax:2 syntax:3)
    // (syntax:1 . (syntax:2 . (syntax:3 . syntax:null))) => (syntax:1 syntax:2 syntax:3)
    public static Object toSyntaxList(Object s) {
        if (RT.isList(s)) {
            return s;
        } else if (RT.isPair(s)) {
            Object cdr = toSyntaxList(RT.cdr(s));
            if (cdr == Boolean.FALSE) {
                return cdr;
            } else {
                return RT.cons(RT.car(s), cdr); // loop
            }
        } else if (s instanceof Syntax) {
            return toSyntaxList(((Syntax) s).e);
        } else {
            return false;
        }
    }

    class ListInput {
        final Values.PList input;
        final Matches matches;
        private int idx = 0;

        ListInput(Values.PList input, Matches matches) {
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

    boolean matchList(Values.PList pattern, ListInput listInput, int depth) {
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

            if (RT.sym(Names.ELLIPSIS).equals(curPatEl) || RT.sym(Names.ELLIPSIS_PLUS).equals(curPatEl)) {
                continue;
            }

            boolean followedByEllipsis = nxtPatEl != null && (
                    RT.sym(Names.ELLIPSIS).equals(nxtPatEl) || RT.sym(Names.ELLIPSIS_PLUS).equals(nxtPatEl)
            );
            boolean zeroMore = followedByEllipsis && RT.sym(Names.ELLIPSIS).equals(nxtPatEl);

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
                    while (listInput.match(curPatEl, depth)) {
                    }
                }
            } else {
                if (!zeroMore) {
                    return false;
                }
            }
        }

        return !listInput.hasNext();
    }

    boolean matchPair(Values.Pair pattern, Values.Pair input, Matches matches, int depth) {
        return match(pattern.car, input.car, matches, depth)
                && match(pattern.cdr, input.cdr, matches, depth);
    }

    boolean matchSymbol(Values.Symbol sym, Object input, Matches matches) {
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

        void add(TreeNode node) {
            nodes.add(node);
        }

        void add(Object node) {
            nodes.add(node);
        }

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

    class Matches implements Pattern.Finder {
        final Map<Values.Symbol, Tree> binding = new LinkedHashMap<>();

        Matches(Object pattern, List<Values.Symbol> formals) {
            Set<Values.Symbol> patternVars = patternVars(pattern, formals);
            for (Values.Symbol name : patternVars) {
                binding.put(name, new Tree(name.name));
            }
        }

        void descend(Set<Values.Symbol> patternVars, int depth) {
            binding.forEach((name, tree) -> {
                if (patternVars.contains(name)) {
                    tree.descend(depth);
                }
            });
        }

        void put(Values.Symbol name, Object value) {
            if (binding.containsKey(name)) {
                binding.get(name).add(value);
            }
        }

        // java 只有异常有 union type, 不好表达自己看着用吧, 不保证类型
        // type T = List<T> | Object
        // type Result = List<Result> | Object
        @Override
        public <T> T get(String name) {
            Tree tree = binding.get(RT.sym(name));
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
                return RT.listColl(lst);
            } else {
                return o;
            }
        }
    }

    Set<Values.Symbol> patternVars(Object pattern, List<Values.Symbol> excluded) {
        Set<Values.Symbol> result = new LinkedHashSet<>();
        patternVars1(pattern, excluded, result);
        return result;
    }

    private void patternVars1(Object pattern, List<Values.Symbol> excluded, Set<Values.Symbol> results) {
        if (pattern instanceof Values.Symbol) {
            Values.Symbol name = (Values.Symbol) pattern;
            if (!excluded.contains(name) && !reserved.contains(name)) {
                results.add(name);
            }
        } else if (pattern instanceof Values.PList) {
            // 特殊处理 quote for case-lambda
            if (isQuote(pattern)) {
                return;
            }

            for (Object el : ((Values.PList) pattern)) {
                patternVars1(el, excluded, results);
            }
        }
    }

    private boolean isQuote(Object pattern) {
        Values.Symbol quote = RT.sym(Names.QUOTE);
        return formals.contains(quote)
                && pattern instanceof Values.PList
                && ((Values.PList) pattern).size() == 2
                && quote.equals(Syntax.toDatum(((Values.PList) pattern).get(0)));
    }

    static boolean isIdentifier(Object pattern) {
        return pattern instanceof Values.Symbol
                || Syntax.isIdentifier(pattern);
    }
}
