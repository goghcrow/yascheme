//package xiao.lang;
//
//import static xiao.lang.Contract.expect;
//import static xiao.lang.RT.*;
//import static xiao.lang.RT.splice;
//
//class Quasiquote {
//
//    public static void main(String[] args) {
////        Object s = Parser.parse1("`(a `(b ,,name1 ,',name2 d) e)");
//
//        Object s = Parser.parse1("`(1 2 ,@a 3)");
//        System.out.println(quasiquote1(cadr(s)));
//        System.out.println(quasiquote2(cadr(s)));
//    }
//
//    static void check(Object x) {
//        expect(isPair(cdr(x)) && isNull(cdr(cdr(x))), "invalid form: " + car(x));
//    }
//
//    final static Values.Symbol quasiquote = sym("quasiquote");
//    final static Values.Symbol quote = sym("quote");
//    final static Values.Symbol unquote = sym("unquote");
//    final static Values.Symbol unquoteSplice = sym("unquote-splicing");
//
//    final static Values.Symbol append = sym("append");
//    final static Values.Symbol listStar = sym("list*");
//    final static Values.Symbol list = sym("list");
//
//
//    final static Values.Symbol dot = sym(".");
//
//    final static Values.PList nul = list(quote, Null());
//
//    static Object quasiquote1(Object x) {
//        if (isPair(x)) {
//            Object car = car(x);
//            Object cdr = cdr(x);
//
//            if (car.equals(quasiquote)) {
//                check(x);
//                Object qq = car(cdr);
//                return quasiquote1(quasiquote1(qq));
//            }
//            if (car.equals(unquote)) {
//                check(x);
//                //noinspection UnnecessaryLocalVariable
//                Object uq = car(cdr);
//                return uq;
//            }
//            // `,@a 不合法
//            if (car.equals(unquoteSplice)) {
//                throw new InterpError("invalid context for unquote-splice");
//            }
//            // 必须是 `(,@a ...)
//            if (isPair(car) && car(car).equals(unquoteSplice)) {
//                check(car);
//                Object splice = cadr(car);
//                Object d = quasiquote1(cdr);
//                if (d.equals(nul)) {
//                    return splice;
//                } else {
//                    return list(append, splice, d);
//                }
//            }
//            Object a = quasiquote1(car);
//            Object d = quasiquote1(cdr);
//            return quasiCons(a, d);
//        } else {
//            return list(quote, x);
//        }
//    }
//
//    static Object quasiCons(Object a, Object d) {
//        if (isPair(d)) {
//            Object card = car(d);
//            Object cdrd = cdr(d);
//            if (card.equals(quote)) {
//                Object qd = car(cdrd);
//                if (isPair(a) && car(a).equals(quote)) {
//                    Object qa = cadr(a);
//                    return list(quote, cons(qa, qd));
//                    // return list(quote, qa, dot, qd);
//                } else {
//                    if (isNull(qd)) {
//                        return list(list, a);
//                    } else {
//                        return list(listStar, a, d);
//                    }
//                }
//            } else if (card.equals(list) || card.equals(listStar)) {
//                return list(card, a, splice(((Values.PList) cdrd)));
//            } else {
//                return list(listStar, a, d);
//            }
//        } else {
//            return list(listStar, a, d);
//        }
//    }
//
//    static Object quasiquote2(Object x) {
//        return quasiquote2(x, 0);
//    }
//
//    static Object quasiquote2(Object x, int n) {
//        if (isPair(x)) {
//            Object car = car(x);
//            Object cdr = cdr(x);
//
//            if (car.equals(quasiquote)) {
//                check(x);
//                return quasiCons(list(quote, quasiquote), quasiquote2(cdr, n + 1));
//            }
//            if (car.equals(unquote)) {
//                check(x);
//                if (n == 0) {
//                    //noinspection UnnecessaryLocalVariable
//                    Object uq = car(cdr);
//                    return uq;
//                } else {
//                    return quasiCons(list(quote, unquote), quasiquote2(cdr, n - 1));
//                }
//            }
//            // `,@a 不合法
//            if (car.equals(unquoteSplice)) {
//                if (n == 0) {
//                    throw new InterpError("invalid context for unquote-splice");
//                } else {
//                    return quasiCons(list(quote, unquoteSplice), quasiquote2(cdr, n - 1));
//                }
//            }
//            // 必须是 `(,@a ...)
//            if (n == 0 && isPair(car) && car(car).equals(unquoteSplice)) {
//                check(car);
//                Object splice = cadr(car);
//                Object d = quasiquote2(cdr, n);
//                if (d.equals(nul)) {
//                    return splice;
//                } else {
//                    return list(append, splice, d);
//                }
//            }
//            Object a = quasiquote2(car, n);
//            Object d = quasiquote2(cdr, n);
//            return quasiCons(a, d);
//        } else {
//            return list(quote, x);
//        }
//    }
//}
