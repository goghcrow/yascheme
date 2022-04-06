package xiao.lang.expander;

import xiao.lang.RT;
import xiao.lang.Values;

import java.util.*;

import static xiao.lang.RT.gensym;
import static xiao.lang.RT.sym;
import static xiao.lang.Values.Symbol;
import static xiao.lang.expander.Binding.LocalBinding;

public class UT {

    public static void main(String[] args) {
        Symbol a = sym("a");
        Symbol b = sym("b");
        Symbol c = sym("c");
        Symbol d = sym("d");
        Symbol f = sym("f");
        Symbol m = sym("m");
        Symbol x = sym("x");
        Symbol y = sym("y");
        Symbol lambda = sym("lambda");
        Symbol cons = sym("cons");
        Symbol car = sym("car");
        // Symbol list = symbol("list");
        Symbol quote = sym("quote");
        Symbol app = sym("#%app");


        // test scope
        {
            Scope sca = Scope.of(null);
            Scope scb = Scope.of(null);
            expectEquals(ScopeSet.of(), ScopeSet.of());
            expectEquals(ScopeSet.of(sca), ScopeSet.of(sca));
            expectEquals(ScopeSet.of(sca, scb), ScopeSet.of(sca, scb));

            expectEquals(ScopeSet.of(sca).equals(ScopeSet.of(sca, scb)), false);
            expectEquals(ScopeSet.of(sca, scb).equals(ScopeSet.of(sca)), false);
            expectEquals(ScopeSet.of(sca).equals(ScopeSet.of(scb)), false);
        }

        // test identifier?
        expectEquals(
                Syntax.isIdentifier(
                        Syntax.of(x)
                ),
                true
        );

        // test datum->syntax
        expectEquals(
                Syntax.fromDatum(null, a),
                Syntax.of(a)
        );
        expectEquals(
                Syntax.fromDatum(null, 1),
                Syntax.of(1)
        );
        expectEquals(
                Syntax.fromDatum(null, list(a, b, c)),
                Syntax.of(list(
                        Syntax.of(a),
                        Syntax.of(b),
                        Syntax.of(c)
                ))
        );
        expectEquals(
                Syntax.fromDatum(null, list(a, Syntax.of(b), c)),
                Syntax.of(list(
                        Syntax.of(a),
                        Syntax.of(b),
                        Syntax.of(c)
                ))
        );

        // test syntax->datum
        expectEquals(
                Syntax.toDatum(
                        Syntax.fromDatum(null, 1)
                ),
                1
        );
        expectEquals(
                Syntax.toDatum(
                        Syntax.fromDatum(null, a)
                ),
                a
        );
        expectEquals(
                Syntax.toDatum(
                        Syntax.fromDatum(null,
                                list(a, b, c)
                        )
                ),
                list(a, b, c)
        );

        // test scope
        Scope sc1 = Scope.of(null);
        Scope sc2 = Scope.of(null);
        expectEquals(
                sc1.equals(sc2),
                false
        );

        // test add-scope
        expectEquals(
                Scope.add(Syntax.of(x), sc1),
                Syntax.of(x, sc1)
        );

        expectEquals(
                Scope.add(
                        Syntax.fromDatum(
                                null,
                                list(x, list(y))
                        ),
                        sc1
                ),
                Syntax.of(list(
                        Syntax.of(x, sc1),
                        Syntax.of(list(
                                Syntax.of(y, sc1)
                        ), sc1)
                ), sc1)
        );
        expectEquals(
                Scope.add(
                        Scope.add(
                                Syntax.of(x),
                                sc1
                        ),
                        sc2
                ),
                Syntax.of(x, sc1, sc2)
        );
        expectEquals(
                Scope.add(
                        Scope.add(
                                Syntax.of(x),
                                sc1
                        ),
                        sc1
                ),
                Syntax.of(x, sc1)
        );

        // test flip-scope
        expectEquals(
                Scope.flip(
                        Syntax.of(x, sc1),
                        sc2
                ),
                Syntax.of(x, sc1, sc2)
        );
        expectEquals(
                Scope.flip(
                        Syntax.of(x, sc1, sc2),
                        sc2
                ),
                Syntax.of(x, sc1)
        );


        // Simulates
        //   (let ([a 1])
        //     (let ([z 2])
        //       ....))
        // where `a` is bound only once
        Binding loc_a = local(a);

        // Simulates
        //   (let ([b 1])
        //     (let ([b 2])
        //       ....))
        // where the inner `b` shadows the outer `b`
        Binding loc_b_out = local(b);
        Binding loc_b_in = local(b);

        // Simulates
        //    (list (let ([c 1]) ...)
        //          (let ([c 2]) ...)))
        // where the `c`s have non-overlaping binding scopes
        Binding loc_c1 = local(c);
        Binding loc_c2 = local(c);

        // Same binding in  sc1  or  sc1 + sc2:
        Scope.bind(Syntax.of(a, sc1), loc_a);

        // Shadowing in sc1 + sc2:
        Scope.bind(Syntax.of(b, sc1), loc_b_out);
        Scope.bind(Syntax.of(b, sc1, sc2), loc_b_in);

        // Ambiguous in sc1 + sc2:
        Scope.bind(Syntax.of(c, sc1), loc_c1);
        Scope.bind(Syntax.of(c, sc2), loc_c2);

        // test resolve
        expectEquals(
                Scope.resolve(Syntax.of(a, sc1)),
                loc_a
        );
        expectEquals(
                Scope.resolve(Syntax.of(a, sc1, sc2)),
                loc_a
        );
        expectEquals(
                Scope.resolve(Syntax.of(a, sc2)),
                null
        );

        expectEquals(
                Scope.resolve(Syntax.of(b, sc1)),
                loc_b_out
        );
        expectEquals(
                Scope.resolve(Syntax.of(b, sc1, sc2)),
                loc_b_in
        );
        expectEquals(
                Scope.resolve(Syntax.of(b, sc2)),
                null
        );

        expectEquals(
                Scope.resolve(Syntax.of(c, sc1)),
                loc_c1
        );
        expectEquals(
                Scope.resolve(Syntax.of(c, sc2)),
                loc_c2
        );
        try {
            Scope.resolve(Syntax.of(c, sc1, sc2));
            throw new RuntimeException();
        } catch (Exception e) {
            if (!e.getMessage().contains("ambiguous")) {
                throw new RuntimeException();
            }
        }

        // test find-all-matching-bindings
        {
            List<Map.Entry<ScopeSet, Binding>> bs = Scope.findAllMatchingBindings(Syntax.of(a, sc1));
            expectEquals(bs.size(), 1);
            expectEquals(bs.get(0).getKey(), Syntax.of(a, sc1).scopes);
            expectEquals(bs.get(0).getValue(), loc_a);
        }
        expectEquals(
                Scope.findAllMatchingBindings(Syntax.of(a, sc2)).size(),
                0
        );
        {
            List<Map.Entry<ScopeSet, Binding>> bs = Scope.findAllMatchingBindings(Syntax.of(b, sc1, sc2));
            expectEquals(bs.size(), 2);
            expectEquals(
                    sets(bs.get(0).getKey(), bs.get(1).getKey()),
                    sets(Syntax.of(b, sc1).scopes, Syntax.of(b, sc1, sc2).scopes)
            );
            expectEquals(
                    sets(bs.get(0).getValue(), bs.get(1).getValue()),
                    sets(loc_b_out, loc_b_in)
            );
        }
        {
            List<Map.Entry<ScopeSet, Binding>> bs = Scope.findAllMatchingBindings(Syntax.of(c, sc1, sc2));
            expectEquals(bs.size(), 2);
            expectEquals(
                    sets(bs.get(0).getKey(), bs.get(1).getKey()),
                    sets(Syntax.of(c, sc1).scopes, Syntax.of(c, sc2).scopes)
            );
            expectEquals(
                    sets(bs.get(0).getValue(), bs.get(1).getValue()),
                    sets(loc_c1, loc_c2)
            );
        }

//        // test check-unambiguous
//        Scope.checkUnambiguous(
//                Syntax.of(b, sc1, sc2),
//                lists(
//                        Syntax.of(b, sc1),
//                        Syntax.of(b, sc1, sc2)
//                ),
//                null
//        );
//        try {
//            Scope.checkUnambiguous(
//                    Syntax.of(c, sc2),
//                    lists(
//                            Syntax.of(b, sc1),
//                            Syntax.of(b, sc2)
//                    ),
//                    null
//            );
//            throw new RuntimeException();
//        } catch (Exception e) {
//            if (!e.getMessage().contains("ambiguous")) {
//                throw new RuntimeException();
//            }
//        }
    }


    static void expectEquals(Object a, Object b, String ...msg) {
        if (!Objects.equals(a, b)) {
            throw new AssertionError(Arrays.toString(msg));
        }
    }

    @SafeVarargs
    static <E> Set<E> sets(E... els) {
        if (els.length == 0) {
            return Collections.emptySet();
        } else {
            Set<E> set = new HashSet<>();
            Collections.addAll(set, els);
            return set;
        }
    }

    static Values.PList list(Object... args) {
        return RT.list(args);
    }

    static Binding local(Symbol sym) {
        return new LocalBinding(gensym(sym));
    }
}
