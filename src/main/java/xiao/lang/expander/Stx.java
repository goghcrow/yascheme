package xiao.lang.expander;

import xiao.lang.RT;

import java.lang.reflect.Array;

/**
 * @author chuxiaofeng
 */
public interface Stx {

    // These utilities facilitate operations on syntax objects.
    // A syntax object that represents a parenthesized sequence
    // can contain a mixture of cons cells and syntax objects,
    // hence the need for `stx-null?', `stx-car', etc.

    // identifier?
    // a syntax identifier?
    static boolean isIdentifier(Object p) {
        return Syntax.isIdentifier(p);
    }

    // stx-null?
    // a syntax null?
    static boolean isNull(Object p) {
        if (p instanceof Syntax) {
            return RT.isNull(((Syntax) p).e);
        } else {
            return RT.isNull(p);
        }
    }

    // stx-null/#f
    // null if a syntax null?, else #f
    static Object isNull1(Object p) {
        if (isNull(p)) {
            return RT.Null();
        } else {
            return false;
        }
    }

    // stx-pair?
    // a syntax pair?
    static boolean isPair(Object p) {
        if (p instanceof Syntax) {
            return RT.isPair(((Syntax) p).e);
        } else {
            return RT.isPair(p);
        }
    }

    // stx-list?
    // a syntax list?
    static boolean isList(Object p) {
        if (RT.isList(p)) {
            return true;
        }
        if (RT.isPair(p)) {
            return isList(RT.cdr(p));
        }
        if (p instanceof Syntax) {
            return isList(((Syntax) p).e);
        } else {
            return false;
        }
    }

    // stx-car
    // car of a syntax pair
    static Object car(Object p) {
        if (p instanceof Syntax) {
            return RT.car(((Syntax) p).e);
        } else {
            return RT.car(p);
        }
    }

    // stx-cdr
    // cdr of a syntax pair
    static Object cdr(Object p) {
        if (p instanceof Syntax) {
            return RT.cdr(((Syntax) p).e);
        } else {
            return RT.cdr(p);
        }
    }

    // stx-cadr
    static Object cadr(Object x) {
        return car(cdr(x));
    }

    // stx->list
    // Flattens a syntax list into a list
    static Object toList(Object e) {
        if (e instanceof Syntax) {
            return Syntax.toList(((Syntax) e));
        }

        Object flatEnd = _toList_loop0(e);
        // 不能等于, 反序列化多个 false
        if (Boolean.FALSE.equals(flatEnd)) {
            // 不是 list, 或者是 list 没有 syntax 元素, 直接返回
            return e;
        } else {
            return _toList_loop1(e, flatEnd);
        }
    }

    static Object _toList_loop0(Object l) {
        if (RT.isNull(l)) {
            return false;
        } else if (RT.isPair(l)) {
            return _toList_loop0(RT.cdr(l));
        } else if (l instanceof Syntax) {
            return Syntax.toList(((Syntax) l));
        } else {
            return false;
        }
    }

    static Object _toList_loop1(Object l, Object flatEnd) {
        if (RT.isNull(l)) {
            return RT.Null();
        } else if (RT.isPair(l)) {
            return RT.cons(RT.car(l), _toList_loop1(RT.cdr(l), flatEnd));
        } else if (l instanceof Syntax) {
            // 返回遇到 Syntax 之后的 list, false 分支已经判断
            assert RT.isList(flatEnd);
            return flatEnd;
        } else {
            return false;
        }
    }

    // stx-vector?
    // a syntax vector?
    static boolean isVector(Object p) {
        return isVector(p, -1);
    }

    // a syntax vector?
    static boolean isVector(Object p, int len) {
        if (p instanceof Syntax) {
            if (RT.isVector(((Syntax) p).e)) {
                if (len >= 0) {
                    return len == Array.getLength(((Syntax) p).e);
                } else {
                    return true;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    // stx-vector-ref
    // syntax vector reference
    static Object vectorRef(Object p, int pos) {
        return Array.get(((Syntax) p).e, pos);
    }

    // stx-vector-length
    // syntax vector reference
    static int vectorLength(Object p) {
        return Array.getLength(((Syntax) p).e);
    }
}
