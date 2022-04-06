package xiao.lang;

import java.lang.reflect.*;
import java.util.*;

import static xiao.lang.Contract.expect;
import static xiao.lang.Interp.Ctx;
import static xiao.lang.Misc.sneakyThrows;

/**
 * Java Interop
 * @author chuxiaofeng
 */
public class Interop {

//    static class Experimental {
//        static Object proxy(Class<?> iface, Map<String, Values.Procedure> methods, Env E) {
//            return proxy(new Class<?>[] { iface }, methods, E);
//        }
//
//        static Object proxy(Class<?>[] interfaces, Map<String, Values.Procedure> methods, Env E) {
//            return Proxy.newProxyInstance(
//                    Interop.class.getClassLoader(),
//                    interfaces,
//                    (proxy, method, args) -> {
//                        String name = method.getName();
//                        expect(methods.containsKey(name), "method missing: " + name);
//
//                        // todo HACK!!! 这里 call/cc 失效!!!
//                        Object[] args1 = args == null ? new Object[0] : args;
//                        Object[] ref = new Object[1];
//                        Interp.Trampoline.run(() -> {
//                            methods.get(name).call(args1, E, v -> {
//                                ref[0] = v;
//                            });
//                        });
//                        return ref[0];
//                    });
//        }
//    }

    interface JavaAccessor {
        void access(Env E, Ctx K);
    }

    static class CallStaticMethod implements JavaAccessor {
        final Class<?> klass;
        final String methodName;
        final Object[] argForms;

        CallStaticMethod(Class<?> klass, String methodName, Object[] argForms) {
            this.klass = klass;
            this.methodName = methodName;
            this.argForms = argForms;
        }

        @Override
        public void access(Env E, Ctx K) {
            if (argForms.length == 0) {
                maybeFieldAccess(E, K);
            } else {
                Interp.interpN(argForms, E, args -> {
                    CallSite callSite = CallSite.method(klass, methodName);
                    Object result = callSite.call(null, ((Object[]) args));
                    K.apply(result);
                });
            }
        }

        // 处理 expansion 中属性访问也被统一处理成方法调用(无法区分)
        // (.instanceMember ClassName) ==> (. ClassName (instanceMember))
        void maybeFieldAccess(Env E, Ctx K) {
            Object result;
            try {
                CallSite callSite = CallSite.method(klass, methodName);
                result = callSite.call(null, new Object[0]);
            } catch (CallSite.MethodMissing e) {
                new AccessStaticField(klass, methodName).access(E, K);
                return;
            }
            K.apply(result);
        }
    }

    static class CallInstanceMethod implements JavaAccessor {
        final Object instance;
        final String methodName;
        final Object[] argForms;

        CallInstanceMethod(Object instance, String methodName, Object[] argForms) {
            this.instance = instance;
            this.methodName = methodName;
            this.argForms = argForms;
        }

        @Override
        public void access(Env E, Ctx K) {
            if (argForms.length == 0) {
                maybeFieldAccess(E, K);
            } else {
                Interp.interpN(argForms, E, args -> {
                    CallSite callSite = CallSite.method(instance.getClass(), methodName);
                    Object result = callSite.call(instance, ((Object[]) args));
                    K.apply(result);
                });
            }
        }

        // 处理 expansion 中属性访问也被统一处理成方法调用(无法区分)
        // (.instanceMember instance-expr) ==> (. instance-expr (instanceMember))
        void maybeFieldAccess(Env E, Ctx K) {
            try {
                CallSite callSite = CallSite.method(instance.getClass(), methodName);
                Object result = callSite.call(instance, new Object[0]);
                K.apply(result);
            } catch (CallSite.MethodMissing e) {
                new AccessInstanceField(instance, methodName).access(E, K);
            }
        }
    }

    static class AccessStaticField implements JavaAccessor {
        final Class<?> klass;
        final String fieldName;

        AccessStaticField(Class<?> klass, String fieldName) {
            this.klass = klass;
            this.fieldName = fieldName;
        }

        @Override
        public void access(Env E, Ctx K) {
            Object value = CallSite.field(klass, fieldName).field(null);
            K.apply(value);
        }
    }

    static class AccessInstanceField implements JavaAccessor {
        final Object instance;
        final String fieldName;

        AccessInstanceField(Object instance, String fieldName) {
            this.instance = instance;
            this.fieldName = fieldName;
        }

        @Override
        public void access(Env E, Ctx K) {
            Object value = CallSite.field(instance.getClass(), fieldName).field(instance);
            K.apply(value);
        }
    }

    static class CallSite {

        @SuppressWarnings("unused")
        public static CallSite constructor(Class<?> klass) {
            // 注意构造函数传类名, 因为 construct.getName() 返回类名
            return method(klass, klass.getName());
        }

        public static CallSite method(Class<?> klass, String name) {
            String key = klass + "::" + name;
            return callSiteCache.computeIfAbsent(key, k -> {
                return new CallSite(klass, name);
            });
        }

        public static CallSite field(Class<?> klass, String name) {
            String key = klass + "." + name;
            return callSiteCache.computeIfAbsent(key, k -> {
                return new CallSite(klass, name);
            });
        }

        final Class<?> klass;
        final String name;

        CallSite(Class<?> klass, String name) {
            assert klass != null;
            this.klass = klass;
            this.name = name;
        }

        @SuppressWarnings("unused")
        // public Object newInstance(Object... args) {
        public Object newInstance(Object[] args) {
            String key = methodKey(args);
            Executable exe = exeInlineCache.computeIfAbsent(key, k -> {
                return dispatchConstructor(args);
            });
            try {
                return ((Constructor<?>) exe).newInstance(exe.isVarArgs() ? varArgs(exe, args) : args);
            } catch (Exception e) {
                sneakyThrows(e);
                return null;
            }
        }

        public Object call(Object obj, Object[] args) {
            String key = methodKey(args);
            // 缓存 not_found 省的 method-missing之后找属性的场景, 每次都找找一遍
            Executable exe = exeInlineCache.computeIfAbsent(key, k -> {
                return dispatchMethod(obj, args);
            });
            if (exe == NOT_FOUND) {
                throw new MethodMissing("方法缺失: " + signature(klass, name, args));
            } else {
                try {
                    return ((Method) exe).invoke(obj, exe.isVarArgs() ? varArgs(exe, args) : args);
                } catch (Exception e) {
                    sneakyThrows(e);
                    return null;
                }
            }
        }

        static Object[] varArgs(Executable exe, Object[] args) {
            return args;

//            // todo 没有处理嵌套数组
//            int minParamCnt = exe.getParameterCount() - 1;
//            Class<?> varType = exe.getParameterTypes()[minParamCnt].getComponentType();
//            Object varArgs = Array.newInstance(varType, args.length - minParamCnt);
//            if (varType.isPrimitive()) {
//                for (int i = minParamCnt; i < args.length; i++) {
//                    // 利用 set 来处理拆箱 !!!
//                    Array.set(varArgs, i - minParamCnt, args[i]);
//                }
//            } else {
//                System.arraycopy(args, minParamCnt, varArgs, 0, args.length - minParamCnt);
//            }
//            Object[] args1 = new Object[minParamCnt + 1];
//            System.arraycopy(args, 0, args1, 0, minParamCnt);
//            args1[minParamCnt] = varArgs;
//            return args1;
        }

        public Object field(Object obj) {
            Field field = fieldInlineCache.computeIfAbsent(filedKey(name), k -> {
                Field field1 = field0(name);
                field1.setAccessible(true);
                return field1;
            });
            try {
                return field.get(obj);
            } catch (Exception e) {
                sneakyThrows(e);
                return null;
            }
        }

        @SuppressWarnings("unused")
        public void field(Object obj, Object value) {
            Field field = fieldInlineCache.computeIfAbsent(filedKey(name), k -> {
                Field field1 = field0(name);
                field1.setAccessible(true);
                return field1;
            });
            try {
                field.set(obj, value);
            } catch (Exception e) {
                sneakyThrows(e);
            }
        }

        String filedKey(String name) {
            return klass.getName() + "." + name;
        }

        String methodKey(Object[] args) {
            StringBuilder buf = new StringBuilder();
            buf.append(klass.getName()).append(".").append(name).append("(");
            for (Object arg : args) {
                if (arg == null) {
                    buf.append("null,");
                } else {
                    buf.append(arg.getClass().getName()).append(",");
                }
            }
            buf.append(")");
            return buf.toString();
        }

        Field field0(String name) {
            try {
                Class<?> c = klass;
                while (c != null) {
                    try {
                        return removeFinal(c.getDeclaredField(name));
                    } catch (NoSuchFieldException ignored) { }
                    c = c.getSuperclass();
                }
                return removeFinal(klass.getField(name)); // do_throw...
            } catch (Exception e) {
                sneakyThrows(e);
                return null;
            }
        }

        Field removeFinal(Field field) throws Exception {
            int acc = field.getModifiers();
            if ((acc & Modifier.FINAL) != 0) {
                Field f = Field.class.getDeclaredField("modifiers");
                f.setAccessible(true);
                f.setInt(field, acc & ~Modifier.FINAL);
            }
            return field;
        }


        // 以下是根据实参进行方法分派的逻辑:
        //
        // 不能直接获取类型树上所有方法, 找到第一个就返回, 因为反射获取的方法是无序的
        // 匹配到哪个方法不确定, 会出各种鬼畜的问题
        // 比如同名方法, 一个静态 一个实例, 签名兼容
        // 方案:
        // 1. 获取类型树上所有方法, 匹配到类型最窄的方法(e.g. "HELLO" 距离 String 比 Object 近, 继承树深度)
        //      准确, but 其实不好判断, 1) 多个参数需要处理, 不同参数最近的可能不一样, 2) 接口带来额外的复杂度
        // 2. 或者如果返回多个直接返回歧义, 非常明确, but 需要人工标注类型, 各种重载会带来带量冲突, 不使用
        // 3. 折中一下, 从子类到父类分阶段找(方法 receiver 动态分派), 避免 父子 override 产生大量歧义,
        //      but 会有问题, 父类可能比子类有更匹配调用参数类型的方法
        //      当前类让然会有大量冲突, 随便选一个???
        // 目前采用第三种
        // todo dispatch 没有处理不定参数, 匹配到数组参数, 构造数组进行调用...
        // todo list 匹配数组 并且转数组

        // 动态分派, 静态分派, 单分派, 多分派

        // 要考虑的点: 1. 构造函数 2. 静态方法, 实例方法 (本质都可以当成静态方法)
        // 3. 不定参数

        static class MethodMissing extends InterpError {
            public MethodMissing(String msg) {
                super(msg);
            }
        }

        Executable dispatchConstructor(Object... args) {
            Class<?> cls = klass;
            expect(!cls.isInterface(), "assert false");
            while (cls != null) { // != Object.class
                Constructor<?>[] constructors = cls.getDeclaredConstructors();
                // 构造函数的 modifiers 不是 static
                List<Executable> exes = matchMethods(constructors, name, args, 0);
                if (exes.isEmpty()) {
                    cls = cls.getSuperclass();
                } else if (exes.size() == 1) {
                    Executable exec = exes.get(0);
                    exec.setAccessible(true);
                    return exec;
                } else {
                    Executable exec = exes.get(0);
                    exec.setAccessible(true);
                    return exec;
//                    throw new InterpError("方法歧义: " + signature(klass, name, args) + "\n"
//                            + exes.stream().map(Object::toString).collect(Collectors.joining("\n")));
                }
            }
            throw new MethodMissing("方法缺失: " + signature(klass, name, args));
        }

        final static Executable NOT_FOUND;
        static {
            Executable exe = null;
//            sun.misc.Unsafe UNSAFE = Reflect.klass(sun.misc.Unsafe.class).field("theUnsafe").get();
            try {
                Constructor<?> ctor = Method.class.getDeclaredConstructors()[0];
                ctor.setAccessible(true);
                exe = (Executable) ctor.newInstance(null, null, null, null, null, 0, 0, null, null, null, null);
//                exe = (Executable) UNSAFE.allocateInstance(Method.class);
            } catch (Exception e) {
                sneakyThrows(e);
            }
            NOT_FOUND = exe;
        }

        Executable dispatchMethod(Object obj, Object... args) {
            int modifiers = (obj == null ? Modifier.STATIC : 0) & (~Modifier.ABSTRACT);
            List<Executable> exes = dispatchMethod0(klass, modifiers, args);
            if (exes.isEmpty()) {
                // computeIfAbsent 不存储 null
                return NOT_FOUND;
            } else if (exes.size() == 1) {
                Executable exec = exes.get(0);
                exec.setAccessible(true);
                return exec;
            } else {
                // todo 实现排序, 挨个参数对比, 找已经匹配的距离 object 最远的
                Executable exec = exes.get(0);
                exec.setAccessible(true);
                return exec;
//                    throw new InterpError("方法歧义: " + signature(klass, name, args) + "\n"
//                            + exes.stream().map(Object::toString).collect(Collectors.joining("\n")));
            }
        }

        // https://docs.oracle.com/javase/specs/jls/se17/html/jls-15.html#jls-15.12.4.4
        // 先找 superclass 再找 direct & indirect superinterface
        List<Executable> dispatchMethod0(Class<?> klass, int modifiers, Object... args) {
            assert klass != null;
            Class<?> cls = klass;
            while (cls != null) {
                Method[] methods = cls.getDeclaredMethods();
                List<Executable> exes = matchMethods(methods, name, args, modifiers);
                if (exes.isEmpty()) {
                    cls = cls.getSuperclass();
                } else {
                    return exes;
                }
            }

            cls = klass;
            while (cls != null) {
                Class<?>[] ifaces = cls.getInterfaces();
                for (Class<?> iface : ifaces) {
                    List<Executable> exes = dispatchMethod0(iface, modifiers, args);
                    if (!exes.isEmpty()) {
                        return exes;
                    }
                }
                cls = cls.getSuperclass();
            }

            return new ArrayList<>();
        }

        List<Executable> matchMethods(
                Executable[] executables,
                String method,
                Object[] args,
                int modifiers // 暂时用作区分静态和实例方法
        ) {
            List<Executable> matches = new ArrayList<>();

            boolean matchVar = false;
            for (Executable m : executables) {
                // 跳过生成的方法, 减少冲突
                if (m.isSynthetic()) {
                    continue;
                }
                boolean modifierMatched = modifiers == 0 || (m.getModifiers() & modifiers) == modifiers;
                if (!modifierMatched) {
                    continue;
                }
                if (!m.getName().equals(method)) {
                    continue;
                }

                // !!!! 这里不支持 varArgs 的语法糖了
                // 如果同时支持传数组和展开，有歧义, 调用时候不好处理
                // boolean varArgs = m.isVarArgs();
                boolean varArgs = false;

                int paramCnt = m.getParameterCount();
                int argCnt = args.length;
                int minVarParamCnt = paramCnt - 1;
                Class<?>[] pTypes = m.getParameterTypes();

                if (varArgs) {
                    if (argCnt < minVarParamCnt) {
                        continue;
                    }
                } else {
                    if (paramCnt != argCnt) {
                        continue;
                    }
                }
                boolean matched = true;

                int n = varArgs ? minVarParamCnt : paramCnt;
                for (int i = 0; i < n; i++) {
                    Object arg = args[i];
                    Class<?> pType = pTypes[i];
                    matched = matchParam(arg, pType);
                    if (!matched) {
                        break;
                    }
                }

                if (matched && varArgs) {
                    Class<?> varArrType = pTypes[paramCnt - 1];
                    expect(varArrType.isArray());
                    Class<?> pType = varArrType.getComponentType();
                    for (int i = minVarParamCnt; i < argCnt; i++) {
                        Object arg = args[i];
                        matched = matchParam(arg, pType);
                        if (!matched) {
                            break;
                        }
                    }
                }

                if (matched) {
                    if (varArgs) {
                        matchVar = true;
                    }
                    matches.add(m);
                }
            }

            // 不定参数方法排到最后匹配
            if (matchVar) {
                matches.sort(Comparator.comparingInt(it -> it.isVarArgs() ? 1 : 0));
            }

            return matches;
        }

        static boolean matchParam(Object arg, Class<?> paramType) {
            if (arg == null) {
                return !paramType.isPrimitive();
            } else if (paramType == Object.class) {
                return true;
            } else {
                // 解释器里的类型都是包装类型, 否则需要对 arg 也进行包装, 统一成包装类型进行比较
                // if (!boxed(paramType).isAssignableFrom(boxed(arg.getClass()))) {
                return boxed(paramType).isInstance(arg);
            }
        }

        static Class<?> boxed(Class<?> c) {
            if (!c.isPrimitive()) return c;
            else if (c == int.class) return Integer.class;
            else if (c == boolean.class) return Boolean.class;
            else if (c == byte.class) return Byte.class;
            else if (c == char.class) return Character.class;
            else if (c == short.class) return Short.class;
            else if (c == long.class) return Long.class;
            else if (c == float.class) return Float.class;
            else if (c == double.class) return Double.class;
            throw new InterpError("不支持 box: " + c);
        }

        static String signature(Class<?> klass, String method, Object[] args) {
            StringBuilder buf = new StringBuilder();
            buf.append(klass).append(".").append(method).append("(");
            boolean isFirst = true;
            for (Object arg : args) {
                if (isFirst) {
                    isFirst = false;
                } else {
                    buf.append(", ");
                }
                buf.append(arg == null ? "null" : arg.getClass().getName());
            }
            buf.append(")");
            return buf.toString();
        }
    }

    static class LRUCache<K, V> extends LinkedHashMap<K, V> {
        final int capacity;

        LRUCache(int capacity) {
            super(capacity + 1, 0.75f, true);
            this.capacity = capacity;
        }

        @Override
        protected boolean
        removeEldestEntry(Map.Entry<K, V> eldest) {
            return (size() > this.capacity);
        }
    }

    final static LRUCache<String, CallSite> callSiteCache = new LRUCache<>(1024);
    final static LRUCache<String, Executable> exeInlineCache = new LRUCache<>(1024);
    final static LRUCache<String, Field> fieldInlineCache = new LRUCache<>(1024);
}
