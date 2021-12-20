package xiao.lang.expander;

import xiao.lang.InterpError;

import java.util.*;
import java.util.function.Function;

import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Procedures.*;
import static xiao.lang.Values.PList;
import static xiao.lang.Values.Symbol;

/**
 * A scope represents a distinct "dimension" of binding.
 * Scope identity is `eq?` identity.
 * @author chuxiaofeng
 */
public class Scope {

    /*
    notation
    -> : map
    * : product
    | : union
    = : type
    . : prop
    () : call-method
    ;; : comment

    scope-id = int
    scope = scope-id
    scope-set = set<scope>
    binding = local-binding | top-level-binding
    local-binding = symbol ;; gensym()
    top-level-binding = symbol ;; core-binding = core-primitive + core-form
     */

    // https://github.com/yjqww6/macrology/blob/master/scope.md
    //
    // ## scope 盘点
    // 对于 phase 无关的各种scope，其自身属性并无区别，主要是按用途来归类。
    // 按syntax-debug-info显示的名字来分，有以下几种scope：
    // - local
    // Fully Expanded Program 的 binding form （let-values、#%plain-lambda等）所引入的scope，用于区分local 的 binding 。
    // 另外，quote-syntax（不带#:local）完全展开时，会删去结果中的local scope。
    // - macro
    // 宏展开引入的scope，通过宏展开前后的两次反转，所有展开过程中新引入的syntax对象都会添加上该scope。
    // - use-site
    // 当一个宏的定义和使用在同一个 definition context 时，宏的参数会带上该scope。
    // - module
    // 展开module时引入的scope。
    // - intdef
    // 展开 internal definition context 时引入的scope。
    // - lifted-require
    // 顾名思义，syntax-local-lift-require的产物。
    // - letrec-body
    // letrec-values/letrec-syntaxes+values添加到其“body”（“rhs”没有）的scope。
    //
    // inside-edge scope和outside-edge scope
    // 在 definition context 展开的时候，输入的syntax对象会带上 outside-edge scope 和 inside-edge scope ；
    // 并且，展开的结果也会带上 inside-edge scope 。
    // outside-edge scope 区分宏引入的 identifier ， inside-edge scope 区分不同的 definition context 。
    // 这两种scope并不是新种类的scope，而是从另一个维度对上述的各种scope进行分类。
    // 对于各种 definition context ：
    //  - top-level的情况：充当 outside-edge scope 的是一个所有top-level共享的scope，即上文出现的#(0 module)；
    //      充当 inside-edge scope 的是一个特定的“multi scope”。namespace-syntax-introduce就是添加这对scope。
    //  - module的情况：充当 outside-edge scope 的是特定的module scope；
    //      充当 inside-edge scope 的是一个特定的“multi scope”。
    //  - internal definition context 的情况：充当 inside-edge scope 的是特定的intdef scope；
    //      充当 outside-edge scope 的是外面的 binding form 特定的local scope，以及letrec-body scope（如果存在）。
    //  - first class internal definition context 的情况和上面类似——除了没有 outside-edge scope ，这也导致了一些卫生问题。
    //
    // ## Scope和Binding 的关系：
    //    Binding as Sets of Scopes中提到
    //    > extends a global table that maps a ⟨symbol, scope set⟩ pair to a representation of a binding.
    //    那么Racket中存在这么一张全局的表吗？按照常识，正经的实现里肯定不会出现一个这么容易内存泄漏的结构。这里就可能导致一些误区：
    //     - 认为 identifier 捕获了自身所在的局部环境——实际上 identifier 仍然只是 symbol + scope set。
    //     - 认为解析一个 identifier 的 binding 依赖当前环境——实际上identifier-binding这个函数并不需要一个namespace参数。
    //    这些看起来暗示着这么一张全局的表的存在，但实际上Racket使用的是一个等效的结构：scope反过来索引了包含了该scope的binding。
    //    把一个 identifier 添加为 binding 的时候， binding 的信息也被记录到其scope set的一个scope里，
    //    这个scope set的超集总是能访问到该 binding 。因此在分析问题时，可以简单地假定这张表存在。
    //
    //    ## binding的解析
    //    Sets of Scopes下的binding解析就是寻找scope set的greatest子集而已，找不到任何子集就“unbound identifier”，
    //    找到多个maximal子集就“identifier's binding is ambiguous”。
    //
    //    ;; 原来版本结构
    //    identifier <: syntax
    //    identifier = symbol * scope-set
    //    bindings = identifier -> binding ;; global binding table
    //    bind(identifier, binding) = bindings.put(identifier, binding)
    //    resolve(identifier):binding = bindings.get(
    //        bindings.identifiers.filter(id =>
    //                identifier.symbol = id.symbol
    //                 and
    //                ;; binding-scopes ⊆ reference-scopes
    //                id.scope-set ⊆ identifier.scope-set
    //        ).max(id => id.scope-set.count)
    //    )
    //
    //    ;; 现在结构
    //    ;; scope 反过来索引了包含了该 scope 的 binding。
    //    ;; 把一个 identifier 添加为 binding 的时候， binding 的信息也被记录到其scope set的一个scope里,
    //    ;; 这个 scope set 的超集总是能访问到该 binding
    //    ;; 给 id 添加 binding 时候, binding 被记录到自身 scope-set 最新的 scope 中, 通过 id 自身的 scope-set 总能访问到该 binding
    //    ;; 其实就相当于 global binding table 搞成一个二级的结构, bindings = symbol -> identifier -> binding, 查找时候先定位 symbol
    //    scope = scope-id * bindings
    //    bindings = symbol -> scope-set -> binding
    //    bind(identifier, binding) := identifier.scope-set.max(sc => sc.scope-id).bindings
    //        .get(identifier.symbol).put(scope-set, binding)
    //    resolve(identifier):binding =
    //
    //
    //    We can attach the bindings for a set of scopes to an arbitrary scope in the set;
    //    we pick the most recently allocated scope to make a binding search
    //    faster and to improve GC, since non-nested binding contexts will
    //    generally not share a most-recent scope.


    public static Scope of() {
        return new Scope();
    }

    // add a scope everywhere, including in nested
    @SuppressWarnings("unchecked")
    public static <T> T add(T s, Scope sc) {
        return ((T) adjustScope(s, set -> set.add(sc)));
    }

    @SuppressWarnings("unchecked")
    public static <T> T remove(T s, Scope sc) {
        return ((T) adjustScope(s, set -> set.remove(sc)));
    }

    public static <T> T remove(T s, ScopeSet scs) {
        for (Scope sc : scs.set) {
            s = remove(s, sc);
        }
        return s;
    }

    @SuppressWarnings("unchecked")
    public static <T> T flip(T s, Scope sc) {
        return ((T) adjustScope(s, set -> set.flip(sc)));
    }

    // Add, remove or flip a scope everywhere (i.e., recurs to nested syntax)
    private static Object adjustScope(Object s, Function<ScopeSet, ScopeSet> mapper) {
        if (s instanceof Syntax) {
            Syntax st = (Syntax) s;
            Object e = adjustScope(st.e, mapper);
            return Syntax.of(e, mapper.apply(st.scopes), st.props);
        } else if (s instanceof PList) {  // fast-route
            return map(el -> adjustScope(el, mapper), ((PList) s));
        } else if (isPair(s)) {
            return cons(
                    adjustScope(car(s), mapper),
                    adjustScope(cdr(s), mapper)
            );
        } else {
            return s;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // Each new scope increments the counter, so we can check whether one
    // scope is newer than another.
    private static int cnt_ = 0;
    final int id = ++cnt_; // internal scope identity; used for sorting

    // sym -> scope-set -> binding
    final Map<Symbol, Map<ScopeSet, Binding>> bindings = new HashMap<>();

    private Scope() { }

    @Override
    public String toString() {
        return "#<scope: " + id + ">";
    }

    /////////////////////////////////////////////////////////////////////////////////////////////

    // add-binding
    public static void bind(Syntax id, Binding binding) {
        expect(Syntax.isIdentifier(id), "expect identifier: " + id);

        ScopeSet scopes = id.scopes;
        Symbol sym = ((Symbol) id.e);

        // add-binding-in-scopes!
        scopes.set.stream()
                .max(Comparator.comparingInt(sc -> sc.id))
                .orElseThrow(() -> new InterpError("cannot bind in empty scope set"))
                .bindings.computeIfAbsent(sym, k -> new HashMap<>())
                .put(scopes, binding);
    }

    // finds the binding for an identifier, returns null if the identifier is unbound
    public static @Nullable Binding resolve(Syntax s) {
        return resolve(s, false);
    }

    @SuppressWarnings("SameParameterValue")
    public static @Nullable Binding resolve(Syntax id, boolean exactly) {
        expect(Syntax.isIdentifier(id), "identifier? " + id);

        ScopeSet scopes = id.scopes;

        List<Map.Entry<ScopeSet, Binding>> candidates = findAllMatchingBindings(id);

        // use candidate with the biggest subset
        if (!candidates.isEmpty()) {
            Map.Entry<ScopeSet, Binding> maxCandidate =
                    candidates.stream().max(Comparator.comparingInt(a -> a.getKey().count())).get();
            checkUnambiguous(maxCandidate, candidates, id, scopes);
            if (!exactly || scopes.count() == maxCandidate.getKey().count()) {
                return maxCandidate.getValue();
            }
        }
        return null;
    }

    // find candidates as bindings with subset of scopes
    // Find all candidate bindings for `id` as the ones with
    // a subset of the scopes of `id`
    // Returns a list of `(cons scope-set binding)`
    static List<Map.Entry<ScopeSet, Binding>> findAllMatchingBindings(Syntax id) {
        ScopeSet scopes = id.scopes;
        Symbol sym = ((Symbol) id.e);

        List<Map.Entry<ScopeSet, Binding>> candidates = new ArrayList<>();
        for (Scope sc : scopes.set) {
            if (sc.bindings.containsKey(sym)) {
                for (Map.Entry<ScopeSet, Binding> it : sc.bindings.get(sym).entrySet()) {
                    // binding scopes ⊆ reference scopes
                    ScopeSet bindingScopes = it.getKey();
                    if (bindingScopes.isSubset(scopes)) {
                        candidates.add(it);
                    }
                }
            }
        }
        return candidates;
    }

    // Check that the binding with the biggest scope set is a superset of all the others
    static void checkUnambiguous(Map.Entry<ScopeSet, Binding> maxCandidate,
                                 List<Map.Entry<ScopeSet, Binding>> candidates,
                                 Syntax id,
                                 ScopeSet scopes) {
        for (Map.Entry<ScopeSet, Binding> c : candidates) {
            // binding scopes ⊆ reference scopes
            ScopeSet referenceScopes = maxCandidate.getKey();
            ScopeSet bindingScopes = c.getKey();
            expect(bindingScopes.isSubset(referenceScopes), "ambiguous: " + id + " " + scopes);
        }
    }

    // If identifiers are `bound-identifier=?`, they are fully
    // interchangeable: same symbol and same scopes
    static boolean isBoundIdentifierEquals(Syntax a, Syntax b) {
        return a.equals(b);
    }
}
