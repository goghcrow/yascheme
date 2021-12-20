package xiao.lang;

import java.util.*;
import java.util.stream.Collectors;

import static xiao.lang.Contract.expect;
import static xiao.lang.Procedures.*;
import static xiao.lang.Values.*;

/*
;; expandName 的 BUG
(let ()
  (define a #f)

  ((syntax-rules ()
     ; a 被处理成文法闭包
     ((_) (lambda a 1))))

  ((syntax-rules ()
     ((_)
       ; a 被处理成文法闭包
       (let ((a 1))
         (.println java.lang.System/out a)))))

  ; redefine
  ;  ((syntax-rules ()
  ;     ; 这里被 a 被处理成文法闭包
  ;     ((_) (define a 1))))

  ((syntax-rules ()
     ; 这里被 a 被处理成文法闭包
     ((_) (set! a 1)))))

; syntax-rules 里头只要没在 matches 中, 且是 name 都会被闭包
; (lambda (id ...) ...), (let ((id expr) ...) ...) (define id expr) (set! id expr) 等
; 中的 id 是标识符 是在宏中定义新的变量, 不能被闭包
*/

/**
 * syntax-rules 卫生宏
 *
 * (syntax-rules (literal-id ...)
 *      [(id . pattern) template] ...)
 *
 * from heist 的ruby 代码修改, 不过卫生宏的实现有 BUG!!!
 * https://github.com/jcoglan/heist/tree/master/lib/heist/runtime/callable/macro
 */
public class Macro {
    final static List<Symbol> reserved = new ArrayList<>();
    static {
        reserved.add(sym(Names.ELLIPSIS));
        reserved.add(sym(Names.ELLIPSIS_PLUS));
        reserved.add(sym(Names.UNDERSCORE));
    }

    static class Rule {
        final Object pattern;
        final Object template;

        Rule(Object pattern, Object template) {
            this.pattern = pattern;
            this.template = template;
        }
    }

    final Env lexicalScope; // defined Scope
    final List<Symbol> formals; // literal-ids, (syntax-rules (literal-ids) ...)
    final List<Rule> rules;
    // final boolean hygienic;

    public Macro(Env lexicalScope, List<Symbol> formals, List<Rule> rules) {
        this.lexicalScope = lexicalScope;
        this.formals = formals;
        this.rules = rules;
    }

    static Set<Symbol> patternVars(Object pattern, List<Symbol> excluded) {
        Set<Symbol> result = new HashSet<>();
        patternVars1(pattern, excluded, result);
        return result;
    }

    private static void patternVars1(Object pattern, List<Symbol> excluded, Set<Symbol> results) {
        if (pattern instanceof Symbol) {
            Symbol name = (Symbol) pattern;
            if (!excluded.contains(name) && !reserved.contains(name)) {
                results.add(name);
            }
        } else if (pattern instanceof PList) {
            for (Object el : ((PList) pattern)) {
                patternVars1(el, excluded, results);
            }
        }
    }

    // 这里是 cells 是 List<Object>, 包装成 PList
    public Expansion matchAndExpand(Object[] cells, Env callScope) {
        for (Rule rule : rules) {
            Matches matches = new Matches(rule.pattern, formals);
            boolean matched = ruleMatches(callScope, rule.pattern, list(cells), matches, 0);
            if (matched) {
                return new Expansion(lexicalScope, callScope, rule.template, matches);
            }
        }
        throw new InterpError("没有匹配的规则: " + Arrays.toString(cells));
    }

    boolean ruleMatches(
            Env callScope,
            Object pattern,
            Object input,
            Matches matches,
            int depth
    ) {
        if (pattern instanceof PList) {
            if (input instanceof PList) {
                ListInput listInput = new ListInput(callScope, ((PList) input), matches);
                return ruleMatchesList(((PList) pattern), listInput, depth);
            } else {
                return false;
            }
        } else if (pattern instanceof Symbol) {
            return ruleMatchesName(callScope, (Symbol) pattern, input, matches);
        } else {
            // literal match
            return pattern.equals(input);
        }
    }

    class ListInput {
        final Env callScope;
        final PList input;
        final Matches matches;
        int idx = 0;

        ListInput(Env callScope, PList input, Matches matches) {
            this.callScope = callScope;
            this.input = input;
            this.matches = matches;
        }

        boolean match(Object patternEl, int depth) {
            if (hasNext()) {
                Object inputEl = next();
                boolean matched = ruleMatches(callScope, patternEl, inputEl, matches, depth);
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

    boolean ruleMatchesList(PList pattern, ListInput listInput, int depth) {
        boolean empty = pattern.isEmpty();
        if (empty) {
            return pattern.equals(listInput.input);
        }

        for (int i = 0; i < pattern.size(); i++) {
            Object curPatEl = pattern.get(i);
            Object nxtPatEl = i + 1 < pattern.size() ? pattern.get(i + 1) : null;


            if (sym(Names.ELLIPSIS).equals(curPatEl)
                    || sym(Names.ELLIPSIS_PLUS).equals(curPatEl)) {
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

//            if (consumed || followedByEllipsis/*...代表可选, 不匹配也行*/) {
//                if (followedByEllipsis && consumed /*之前没有匹配成功, 之后也不会*/) {
//                    //noinspection StatementWithEmptyBody
//                    while (listInput.match(curPatEl, depth)) { } /*匹配 ... 重复*/
//                }
//            } else {
//                return false;
//            }

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

    boolean ruleMatchesName(Env callScope, Symbol name, Object input, Matches matches) {
        if (formals.contains(name)) {
            if (name.equals(input)) {
                return lexicalScope.findDefinedScope(name.name) == callScope.findDefinedScope(((Symbol) input).name);
            } else {
                return false;
            }
        } else {
            matches.put(name, input);
            return true;
        }
    }




    static class TreeNode {
        List<Object/*TreeNode|Node*/> nodes = new ArrayList<>();

        TreeNode last() {
            expect(!nodes.isEmpty(), "!nodes.isEmpty()");
            Object last = nodes.get(nodes.size() - 1);
            expect(last instanceof TreeNode, "last instance TreeNode");
            return ((TreeNode) last);
        }

        TreeNode getTreeNode(int idx) {
            Object treeNode = nodes.get(idx);
            expect(treeNode instanceof TreeNode, "treeNode instanceof TreeNode");
            return ((TreeNode) treeNode);
        }

        Object getNode(int idx) {
            return nodes.get(idx);
        }

        int size() {
            return nodes.size();
        }

        void add(TreeNode node) {
            nodes.add(node);
        }

        void add(Object node) {
            nodes.add(node);
        }

        @Override
        public String toString() {
            return nodes.toString();
        }
    }

    // Ast.Node 或者说 form 只保存在叶子节点
    static class Tree {
        final String name;
        int depth = 0; // 只增不减
        TreeNode root = new TreeNode();
        int[] indexes; // depth => read_index_when_expand

        public Tree(String name) {
            this.name = name;
        }

        // for match, 重复深度下降
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

        // for match
        void add(Object value) {
            tail(depth).add(value);
        }

        // for match
        private TreeNode tail(int depth) {
            TreeNode node = root;
            for (int i = 0; i < depth; i++) {
                node = node.last();
            }
            return node;
        }

        // for expand
        // 展开时往前移动读游标, 到终点移回来(同一个变量可以被多次使用)
        // 因为展开时是递归的深度遍历, 所以读游标的是先叶子节点消费完,
        // 然后父节点移动一格, 再消费完叶子节点, 依次类推
        void shift(int depth) {
            if (depth <= this.depth) {
                indexes[depth] += 1;
                if (indexes[depth] >= current(depth).size()) {
                    indexes[depth] = 0;
                }
            }
        }

        // for expand
        // 当前层读游标位置的元素
        // 这里假设一定 current(depth) 一定是 TreeNode, getNode(readIdx) 一定是 Node
        Object read() {
            int readIdx = indexes[depth];
            return current(depth).getNode(readIdx);
        }

        // for expand
        int size(int depth) {
            if (depth > this.depth) {
                return -1;
            } else {
                // try { } catch (Exception e) { return 0; }
                return current(depth).size();
            }
        }

        // for expand
        // 当前层读游标位置的 TreeNode 节点 (这里假定一定不是叶子节点, 叶子节点是 Node)
        private TreeNode current(int depth) {
            TreeNode node = root;
            for (int i = 0; i < depth; i++) {
                int idx = indexes[i];
                node = node.getTreeNode(idx);
            }
            return node;
        }

        // 必须再 match 完之后, 算完 depth 之后调用
        private void initIndexes() {
            indexes = new int[depth + 1];
        }

        @Override
        public String toString() {
            return root.toString();
        }
    }

    static class Matches {
        final Map<Symbol, Tree> binding = new LinkedHashMap<>();

        Matches(Object pattern, List<Symbol> formals) {
            Set<Symbol> patternVars = Macro.patternVars(pattern, formals);
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
            if (has(name)) {
                binding.get(name).add(value);
            }
        }

        boolean has(Symbol name) {
            return binding.containsKey(name);
        }

        Object get(Symbol name) {
            return binding.get(name).read();
        }

        void expand(Object template, int depth, Runnable f) {
            Set<Symbol> patternVars = Macro.patternVars(template, new ArrayList<>());
            int sz = size(patternVars, depth);
            for (int i = 0; i < sz; i++) {
                f.run();
                iterate(patternVars, depth);
            }
        }

        private int size(Set<Symbol> patternVars, int depth) {
            int result = -1;
            for (Map.Entry<Symbol, Tree> it : binding.entrySet()) {
                Symbol name = it.getKey();
                if (!patternVars.contains(name)) {
                    continue;
                }

                Tree tree = it.getValue();
                int size = tree.size(depth);
                if (size == -1) {
                    continue;
                }

                if (result == -1) {
                    result = size;
                } else {
                    expect(result == size, "重复模式不匹配");
                }
            }
            expect(result != -1, "重复模式不匹配");
            return result;
        }

        // 一组变量整体移动读游标
        private void iterate(Set<Symbol> patternVars, int depth) {
            binding.forEach((name, tree) -> {
                if (patternVars.contains(name)) {
                    tree.shift(depth);
                }
            });
        }

        private void beforeExpand() {
            binding.forEach((name, tree) -> tree.initIndexes());
        }

        @Override
        public String toString() {
            // return data.toString();
            return binding.entrySet().stream().map(Object::toString).collect(Collectors.joining("\n"));
        }
    }

    public static class Expansion {
        public final Env lexicalScope;
        public final Env callingScope;
        public final Object expression;

        final Matches matches;

        public Expansion(
                Env lexicalScope,
                Env callingScope,
                Object template,
                Matches matches
        ) {
            this.lexicalScope = lexicalScope;
            this.callingScope = callingScope;
            this.matches = matches;

            this.matches.beforeExpand();
            this.expression = expand(template, 0, false);
        }

        Object expand(Object template, int depth, boolean ignoringEllipses) {
            if (template instanceof PList) {
                return expandList(((PList) template), depth, ignoringEllipses);
            } else if (template instanceof Symbol) {
                return expandName(((Symbol) template));
            } else {
                return template;
            }
        }

        Object expandList(PList lst, int depth, boolean ignoringEllipses) {
            if (lst.isEmpty()) {
                return lst;
            }

            // (... template) 形式的模板与 template 是等价的除非模板中的省略号没有什么特殊含义。
            // 也就是说，模板中的省略号被当作普通标识符。
            // 特别地，(... ...) 会产生 ... ，这也就允许语法扩展来展开得到含有省略号的形式。
            Object car = lst.get(0);
            if (sym(Names.ELLIPSIS).equals(car) || sym(Names.ELLIPSIS_PLUS).equals(car)) {
                expect(lst.size() == 2, "... 语法错误");
                return expand(lst.get(1), depth, true);
            }

            Object[] repeaterRef = new Object[1]; //
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < lst.size(); i++) {
                Object cur = lst.get(i);
                Object nxt = i + 1 < lst.size() ? lst.get(i + 1) : null;

                boolean followedByEllipsis = nxt != null && (
                        sym(Names.ELLIPSIS).equals(nxt) || sym(Names.ELLIPSIS_PLUS).equals(nxt)
                );
                if (followedByEllipsis && !ignoringEllipses) {
                    repeaterRef[0] = cur;
                }

                // 展开是遇到重复, 说明 patternVar 的值存储在子树中, 所以 dep + 1

                if ((sym(Names.ELLIPSIS).equals(cur) || sym(Names.ELLIPSIS_PLUS).equals(cur))
                        && !ignoringEllipses) {
                    matches.expand(repeaterRef[0], depth + 1, () -> {
                        Object expanded = expand(repeaterRef[0], depth + 1, false);
                        result.add(expanded);
                    });
                } else if (followedByEllipsis && !ignoringEllipses) {
                    // followedByEllipsis 延迟到下个循环 cur == ELLIPSIS 循环执行替换
                    // repeaterRef[0] = cur;
                    //noinspection UnnecessaryContinue
                    continue;
                } else {
                    // ignoringEllipses = true 分支
                    // ignoringEllipses = false && (cur != ELLIPSIS || !followedByEllipsis) 分支
                    Object expanded = expand(cur, depth, ignoringEllipses);
                    result.add(expanded);
                }
            }

            return listColl(result);
        }

        // !!!!! 注意宏中的临时变量不要与宏定义域作用域的变量重名 !!!!!
        // 会变成 SyntacticClosureNode
        Object expandName(Symbol name) {
            if (matches.has(name)) {
                return matches.get(name);
            }

            if (lexicalScope.defined(name.name)) {
                return new SyntacticClosure(lexicalScope, name);
            } else {
                return sym(rename(name.name));
            }
        }

        String rename(String id) {
            if (callingScope.defined(id)) {
                int i = 1;
                while (callingScope.defined(id + i)) {
                    i++;
                }
                return id + i;
            } else {
                return id;
            }
        }
    }
}
