package xiao.lang2;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static xiao.lang2.Contract.expect;
import static xiao.lang2.Interp.Ctx;
import static xiao.lang2.Misc.sneakyThrows;

/**
 * @author chuxiaofeng
 */
public class JavaInterop {

    static class Experimental {
        static Object proxy(Class<?> iface, Map<String, Values.Procedure> methods, Env E) {
            return proxy(new Class<?>[] { iface }, methods, E);
        }

        static Object proxy(Class<?>[] interfaces, Map<String, Values.Procedure> methods, Env E) {
            return Proxy.newProxyInstance(
                    JavaInterop.class.getClassLoader(),
                    interfaces,
                    (proxy, method, args) -> {
                        String name = method.getName();
                        expect(methods.containsKey(name), "method missing: " + name);

                        // todo HACK!!! 这里 call/cc 失效!!!
                        Object[] args1 = args == null ? new Object[0] : args;
                        Object[] ref = new Object[1];
                        Interp.Trampoline.run(() -> {
                            methods.get(name).call(args1, E, v -> {
                                ref[0] = v;
                            });
                        });
                        return ref[0];
                    });
        }
    }

    interface JavaAccessor {
        void access(Interp interp, Env E, Ctx K);
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
        public void access(Interp interp, Env E, Ctx K) {
            interp.interpN(argForms, E, args -> {
                CallSite callSite = CallSite.method(klass, methodName);
                Object result = callSite.call(null, ((Object[]) args));
                K.apply(result);
            });
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
        public void access(Interp interp, Env E, Ctx K) {
            interp.interpN(argForms, E, args -> {
                CallSite callSite = CallSite.method(instance.getClass(), methodName);
                Object result = callSite.call(instance, ((Object[]) args));
                K.apply(result);
            });
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
        public void access(Interp interp, Env E, Ctx K) {
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
        public void access(Interp interp, Env E, Ctx K) {
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
                return pickConstructor(args);
            });
            try {
                return ((Constructor<?>) exe).newInstance(args);
            } catch (Exception e) {
                sneakyThrows(e);
                return null;
            }
        }

        // public Object call(Object obj, Object... args) {
        public Object call(Object obj, Object[] args) {
            String key = methodKey(args);
            Executable exe = exeInlineCache.computeIfAbsent(key, k -> {
                return pickMethod(obj, args);
            });
            try {
                return ((Method) exe).invoke(obj, args);
            } catch (Exception e) {
                sneakyThrows(e);
                return null;
            }
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

        Executable pickConstructor(Object... args) {
            Class<?> cls = klass;
            expect(!cls.isInterface(), "assert false");
            while (cls != null) { // != Object.class
                Constructor<?>[] constructors = cls.getDeclaredConstructors();
                // 构造函数的 modifiers 不是 static
                List<Executable> exes = dispatch(constructors, name, args, 0);
                if (exes.isEmpty()) {
                    cls = cls.getSuperclass();
                } else if (exes.size() == 1) {
                    Executable exec = exes.get(0);
                    exec.setAccessible(true);
                    return exec;
                } else {
                    throw new InterpError("方法歧义: " + signature(klass, name, args) + "\n"
                            + exes.stream().map(Object::toString).collect(Collectors.joining("\n")));
                }
            }
            throw new InterpError("方法缺失: " + signature(klass, name, args));
        }

        Executable pickMethod(Object obj, Object... args) {
            int modifiers = obj == null ? Modifier.STATIC : 0;
            Class<?> cls = klass;
            while (cls != null) {
                Method[] methods = cls.getDeclaredMethods();
                List<Executable> exes = dispatch(methods, name, args, modifiers);
                if (exes.isEmpty()) {
                    cls = cls.getSuperclass();
                } else if (exes.size() == 1) {
                    Executable exec = exes.get(0);
                    exec.setAccessible(true);
                    return exec;
                } else {
                    // todo 实现排序, 挨个参数对比, 找已经匹配的距离 object 最远的
                    return exes.get(0);
//                    throw new InterpError("方法歧义: " + signature(klass, name, args) + "\n"
//                            + exes.stream().map(Object::toString).collect(Collectors.joining("\n")));
                }
            }
            throw new InterpError("方法缺失: " + signature(klass, name, args));
        }

        List<Executable> dispatch(
                Executable[] executables,
                String method,
                Object[] args,
                int modifiers // 暂时用作区分静态和实例方法
        ) {
            List<Executable> matches = new ArrayList<>();

            for (Executable m : executables) {
                // 跳过生成的方法, 减少冲突
                if (m.isSynthetic()) {
                    continue;
                }

                if (
                        (modifiers == 0 || (m.getModifiers() & modifiers) != 0)
                        && m.getName().equals(method)
                        && m.getParameterCount() == args.length
                ) {
                    Class<?>[] pTypes = m.getParameterTypes();
                    boolean matched = true;
                    for (int i = 0; i < args.length; i++) {
                        Object arg = args[i];
                        Class<?> pType = pTypes[i];
                        if (arg == null) {
                            if (pType.isPrimitive()) {
                                matched = false;
                                break;
                            }
                        } else {
                            // 解释器里的类型都是包装类型, 否则需要对 arg 也进行包装, 统一成包装类型进行比较
                            // if (!boxed(pType).isAssignableFrom(boxed(arg.getClass()))) {
                            if (!boxed(pType).isInstance(arg)) {
                                matched = false;
                                break;
                            }
                        }
                    }
                    if (matched) {
                        matches.add(m);
                    }
                }
            }

            return matches;
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
