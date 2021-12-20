package xiao.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * generated code
 * @author chuxiaofeng
 */
@SuppressWarnings("unused")
public class Generated {

    public static void main(String[] args) {
        genCoercePrimitive();
        genBinaryNumericalMethods();
    }

    static void genCoercePrimitive() {
        String[] numTypes = { "byte", "short", "int", "long", "float", "double", "char", "BigInteger", "BigDecimal" };
        String[] numTypes1 = { "Byte", "Short", "Integer", "Long", "Float", "Double", "Character", "BigInteger", "BigDecimal" };

        for (String numType : numTypes) {
            String methodName = numType.substring(0, 1).toUpperCase() + numType.substring(1);
            if (numType.equals("BigInteger")) {
                System.out.printf("(define (bigint a) (xiao.lang.Generated/castTo%s a))\n",
                        methodName);
            } else if (numType.equals("BigDecimal")) {
                System.out.printf("(define (bigdec a) (xiao.lang.Generated/castTo%s a))\n",
                        methodName);
            } else {
                System.out.printf("(define (%s a) (xiao.lang.Generated/castTo%s a))\n",
                        numType,
                        methodName);
            }
        }
        System.out.println();

        for (int i = 0; i < numTypes.length; i++) {
            String srcType = "String";
            String dstType = numTypes[i];
            if (dstType.equals("char")) {
                continue;
            }
            String methodName = dstType.substring(0, 1).toUpperCase() + dstType.substring(1);
            if (dstType.startsWith("Big")) {
                System.out.printf("public static %s castTo%s(%s x) { return new %s(x); }\n",
                        dstType,
                        methodName,
                        srcType,
                        dstType
                );
            } else {
                System.out.printf("public static %s castTo%s(%s x) { return %s.parse%s(x); }\n",
                        dstType,
                        methodName,
                        srcType,
                        numTypes1[i],
                        numTypes1[i]
                );
            }
        }

        for (String srcType : numTypes) {
            for (String dstType : numTypes) {
                String methodName = dstType.substring(0, 1).toUpperCase() + dstType.substring(1);
                if (srcType.equals(dstType)) {
                    if (srcType.startsWith("Big")) {
                        System.out.printf("public static %s castTo%s(%s x) { return x; }\n",
                                dstType,
                                methodName,
                                srcType);
                    } else {
                        System.out.printf("public static %s castTo%s(%s x) { return x; }\n",
                                dstType,
                                methodName,
                                srcType);
                    }
                    continue;
                }

                if (srcType.startsWith("Big") && !dstType.startsWith("Big")) {
                    // big -> primitive
                    System.out.printf("public static %s castTo%s(%s x) { return x.%sValue(); }\n",
                            dstType,
                            methodName,
                            srcType,
                            dstType);
                    continue;
                }

                if (!srcType.startsWith("Big") && dstType.startsWith("Big")) {
                    // primitive -> big
                    System.out.printf("public static %s castTo%s(%s x) { return new %s(String.valueOf(x)); }\n",
                            dstType,
                            methodName,
                            srcType,
                            dstType);
                    continue;
                }

                if (srcType.equals("BigInteger")) {
                    if (dstType.equals("BigDecimal")) {
                        // bigint -> bigdec
                        System.out.printf("public static %s castTo%s(%s x) { return new BigDecimal(x); }\n",
                                dstType,
                                methodName,
                                srcType
                        );
                        continue;
                    }
                } else if (srcType.equals("BigDecimal")) {
                    // bigdec -> bigint
                    if (dstType.equals("BigInteger")) {
                        System.out.printf("public static %s castTo%s(%s x) { return x.toBigInteger(); }\n",
                                dstType,
                                methodName,
                                srcType
                        );
                        continue;
                    }
                }

                System.out.printf("public static %s castTo%s(%s x) { return (%s) x; }\n",
                        dstType,
                        methodName,
                        srcType,
                        dstType);
            }
        }
    }

    static void genBinaryNumericalMethods() {
        String[] numTypes = {
                "byte", "short", "int", "long", "float", "double", "BigInteger", "BigDecimal"
        };
        // 二元操作数方法
        String[][] methods = {
                // method-name, return-type, operator, bigXXX-method
                { "add", "Number", "+", "add" },
                { "sub", "Number", "-", "subtract" },
                { "mul", "Number", "*", "multiply" },
                { "div", "Number", "/", "divide" },
                { "eq", "boolean", "==", "equals" },
                { "lt", "boolean", "<", "compareTo", "< 0" },
                { "gt", "boolean", ">", "compareTo", "> 0" },
                { "modulo", "Number", "%", "remainder"},
        };


        for (String[] signature : methods) {
            String method = signature[0];
            String rType = signature[1];
            String operator = signature[2];
            boolean isCmp = "lt".equals(method) || "gt".equals(method);
            for (String pType1 : numTypes) {
                for (String pType2 : numTypes) {
                    if (pType1.startsWith("Big") && pType2.startsWith("Big")) {
                        String bigMethod = signature[3];
                        if (pType1.equals(pType2)) {
                            if (isCmp) {
                                String cmpZero = signature[4];
                                System.out.printf("public static %s %s(%s x, %s y) { return x.%s(y) %s; }\n",
                                        rType, method, pType1, pType2, bigMethod, cmpZero);
                            } else {
                                System.out.printf("public static %s %s(%s x, %s y) { return x.%s(y); }\n",
                                        rType, method, pType1, pType2, bigMethod);
                            }
                        } else if (pType2.equals("BigDecimal")) {
                            if (isCmp) {
                                String cmpZero = signature[4];
                                System.out.printf("public static %s %s(%s x, %s y) { return (new %s(String.valueOf(x)).%s(y)) %s; }\n",
                                        rType, method, pType1, pType2, pType2, bigMethod, cmpZero);
                            } else {
                                System.out.printf("public static %s %s(%s x, %s y) { return new %s(String.valueOf(x)).%s(y); }\n",
                                        rType, method, pType1, pType2, pType2, bigMethod);
                            }
                        } else {
                            if (isCmp) {
                                String cmpZero = signature[4];
                                System.out.printf("public static %s %s(%s x, %s y) { return (x.%s(new %s(String.valueOf(y)))) %s; }\n",
                                        rType, method, pType1, pType2, bigMethod, pType1, cmpZero);
                            } else {
                                System.out.printf("public static %s %s(%s x, %s y) { return x.%s(new %s(String.valueOf(y))); }\n",
                                        rType, method, pType1, pType2, bigMethod, pType1);
                            }
                        }
                    } else if (pType1.startsWith("Big")) {
                        String bigMethod = signature[3];
                        if (isCmp) {
                            String cmpZero = signature[4];
                            System.out.printf("public static %s %s(%s x, %s y) { return (x.%s(new %s(String.valueOf(y)))) %s; }\n",
                                    rType, method, pType1, pType2, bigMethod, pType1, cmpZero);
                        } else {
                            System.out.printf("public static %s %s(%s x, %s y) { return x.%s(new %s(String.valueOf(y))); }\n",
                                    rType, method, pType1, pType2, bigMethod, pType1);
                        }
                    } else if (pType2.startsWith("Big")) {
                        String bigMethod = signature[3];
                        if (isCmp) {
                            String cmpZero = signature[4];
                            System.out.printf("public static %s %s(%s x, %s y) { return (new %s(String.valueOf(x)).%s(y)) %s; }\n",
                                    rType, method, pType1, pType2, pType2, bigMethod, cmpZero);
                        } else {
                            System.out.printf("public static %s %s(%s x, %s y) { return new %s(String.valueOf(x)).%s(y); }\n",
                                    rType, method, pType1, pType2, pType2, bigMethod);
                        }
                    } else {
                        System.out.printf("public static %s %s(%s x, %s y) { return x %s y; }\n", rType, method, pType1, pType2, operator);
                    }
                }
            }
        }
    }


    public static byte castToByte(String x) { return Byte.parseByte(x); }
    public static short castToShort(String x) { return Short.parseShort(x); }
    public static int castToInt(String x) { return Integer.parseInt(x); }
    public static long castToLong(String x) { return Long.parseLong(x); }
    public static float castToFloat(String x) { return Float.parseFloat(x); }
    public static double castToDouble(String x) { return Double.parseDouble(x); }
    public static BigInteger castToBigInteger(String x) { return new BigInteger(x); }
    public static BigDecimal castToBigDecimal(String x) { return new BigDecimal(x); }
    public static byte castToByte(byte x) { return x; }
    public static short castToShort(byte x) { return (short) x; }
    public static int castToInt(byte x) { return (int) x; }
    public static long castToLong(byte x) { return (long) x; }
    public static float castToFloat(byte x) { return (float) x; }
    public static double castToDouble(byte x) { return (double) x; }
    public static char castToChar(byte x) { return (char) x; }
    public static BigInteger castToBigInteger(byte x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(byte x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(short x) { return (byte) x; }
    public static short castToShort(short x) { return x; }
    public static int castToInt(short x) { return (int) x; }
    public static long castToLong(short x) { return (long) x; }
    public static float castToFloat(short x) { return (float) x; }
    public static double castToDouble(short x) { return (double) x; }
    public static char castToChar(short x) { return (char) x; }
    public static BigInteger castToBigInteger(short x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(short x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(int x) { return (byte) x; }
    public static short castToShort(int x) { return (short) x; }
    public static int castToInt(int x) { return x; }
    public static long castToLong(int x) { return (long) x; }
    public static float castToFloat(int x) { return (float) x; }
    public static double castToDouble(int x) { return (double) x; }
    public static char castToChar(int x) { return (char) x; }
    public static BigInteger castToBigInteger(int x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(int x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(long x) { return (byte) x; }
    public static short castToShort(long x) { return (short) x; }
    public static int castToInt(long x) { return (int) x; }
    public static long castToLong(long x) { return x; }
    public static float castToFloat(long x) { return (float) x; }
    public static double castToDouble(long x) { return (double) x; }
    public static char castToChar(long x) { return (char) x; }
    public static BigInteger castToBigInteger(long x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(long x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(float x) { return (byte) x; }
    public static short castToShort(float x) { return (short) x; }
    public static int castToInt(float x) { return (int) x; }
    public static long castToLong(float x) { return (long) x; }
    public static float castToFloat(float x) { return x; }
    public static double castToDouble(float x) { return (double) x; }
    public static char castToChar(float x) { return (char) x; }
    public static BigInteger castToBigInteger(float x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(float x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(double x) { return (byte) x; }
    public static short castToShort(double x) { return (short) x; }
    public static int castToInt(double x) { return (int) x; }
    public static long castToLong(double x) { return (long) x; }
    public static float castToFloat(double x) { return (float) x; }
    public static double castToDouble(double x) { return x; }
    public static char castToChar(double x) { return (char) x; }
    public static BigInteger castToBigInteger(double x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(double x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(char x) { return (byte) x; }
    public static short castToShort(char x) { return (short) x; }
    public static int castToInt(char x) { return (int) x; }
    public static long castToLong(char x) { return (long) x; }
    public static float castToFloat(char x) { return (float) x; }
    public static double castToDouble(char x) { return (double) x; }
    public static char castToChar(char x) { return x; }
    public static BigInteger castToBigInteger(char x) { return new BigInteger(String.valueOf(x)); }
    public static BigDecimal castToBigDecimal(char x) { return new BigDecimal(String.valueOf(x)); }
    public static byte castToByte(BigInteger x) { return x.byteValue(); }
    public static short castToShort(BigInteger x) { return x.shortValue(); }
    public static int castToInt(BigInteger x) { return x.intValue(); }
    public static long castToLong(BigInteger x) { return x.longValue(); }
    public static float castToFloat(BigInteger x) { return x.floatValue(); }
    public static double castToDouble(BigInteger x) { return x.doubleValue(); }
    public static BigInteger castToBigInteger(BigInteger x) { return x; }
    public static BigDecimal castToBigDecimal(BigInteger x) { return new BigDecimal(x); }
    public static byte castToByte(BigDecimal x) { return x.byteValue(); }
    public static short castToShort(BigDecimal x) { return x.shortValue(); }
    public static int castToInt(BigDecimal x) { return x.intValue(); }
    public static long castToLong(BigDecimal x) { return x.longValue(); }
    public static float castToFloat(BigDecimal x) { return x.floatValue(); }
    public static double castToDouble(BigDecimal x) { return x.doubleValue(); }
    public static BigInteger castToBigInteger(BigDecimal x) { return x.toBigInteger(); }
    public static BigDecimal castToBigDecimal(BigDecimal x) { return x; }


    public static Number add(byte x, byte y) { return x + y; }
    public static Number add(byte x, short y) { return x + y; }
    public static Number add(byte x, int y) { return x + y; }
    public static Number add(byte x, long y) { return x + y; }
    public static Number add(byte x, float y) { return x + y; }
    public static Number add(byte x, double y) { return x + y; }
    public static Number add(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(short x, byte y) { return x + y; }
    public static Number add(short x, short y) { return x + y; }
    public static Number add(short x, int y) { return x + y; }
    public static Number add(short x, long y) { return x + y; }
    public static Number add(short x, float y) { return x + y; }
    public static Number add(short x, double y) { return x + y; }
    public static Number add(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(int x, byte y) { return x + y; }
    public static Number add(int x, short y) { return x + y; }
    public static Number add(int x, int y) { return x + y; }
    public static Number add(int x, long y) { return x + y; }
    public static Number add(int x, float y) { return x + y; }
    public static Number add(int x, double y) { return x + y; }
    public static Number add(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(long x, byte y) { return x + y; }
    public static Number add(long x, short y) { return x + y; }
    public static Number add(long x, int y) { return x + y; }
    public static Number add(long x, long y) { return x + y; }
    public static Number add(long x, float y) { return x + y; }
    public static Number add(long x, double y) { return x + y; }
    public static Number add(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(float x, byte y) { return x + y; }
    public static Number add(float x, short y) { return x + y; }
    public static Number add(float x, int y) { return x + y; }
    public static Number add(float x, long y) { return x + y; }
    public static Number add(float x, float y) { return x + y; }
    public static Number add(float x, double y) { return x + y; }
    public static Number add(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(double x, byte y) { return x + y; }
    public static Number add(double x, short y) { return x + y; }
    public static Number add(double x, int y) { return x + y; }
    public static Number add(double x, long y) { return x + y; }
    public static Number add(double x, float y) { return x + y; }
    public static Number add(double x, double y) { return x + y; }
    public static Number add(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).add(y); }
    public static Number add(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(BigInteger x, byte y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, short y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, int y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, long y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, float y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, double y) { return x.add(new BigInteger(String.valueOf(y))); }
    public static Number add(BigInteger x, BigInteger y) { return x.add(y); }
    public static Number add(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).add(y); }
    public static Number add(BigDecimal x, byte y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, short y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, int y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, long y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, float y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, double y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, BigInteger y) { return x.add(new BigDecimal(String.valueOf(y))); }
    public static Number add(BigDecimal x, BigDecimal y) { return x.add(y); }
    public static Number sub(byte x, byte y) { return x - y; }
    public static Number sub(byte x, short y) { return x - y; }
    public static Number sub(byte x, int y) { return x - y; }
    public static Number sub(byte x, long y) { return x - y; }
    public static Number sub(byte x, float y) { return x - y; }
    public static Number sub(byte x, double y) { return x - y; }
    public static Number sub(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(short x, byte y) { return x - y; }
    public static Number sub(short x, short y) { return x - y; }
    public static Number sub(short x, int y) { return x - y; }
    public static Number sub(short x, long y) { return x - y; }
    public static Number sub(short x, float y) { return x - y; }
    public static Number sub(short x, double y) { return x - y; }
    public static Number sub(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(int x, byte y) { return x - y; }
    public static Number sub(int x, short y) { return x - y; }
    public static Number sub(int x, int y) { return x - y; }
    public static Number sub(int x, long y) { return x - y; }
    public static Number sub(int x, float y) { return x - y; }
    public static Number sub(int x, double y) { return x - y; }
    public static Number sub(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(long x, byte y) { return x - y; }
    public static Number sub(long x, short y) { return x - y; }
    public static Number sub(long x, int y) { return x - y; }
    public static Number sub(long x, long y) { return x - y; }
    public static Number sub(long x, float y) { return x - y; }
    public static Number sub(long x, double y) { return x - y; }
    public static Number sub(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(float x, byte y) { return x - y; }
    public static Number sub(float x, short y) { return x - y; }
    public static Number sub(float x, int y) { return x - y; }
    public static Number sub(float x, long y) { return x - y; }
    public static Number sub(float x, float y) { return x - y; }
    public static Number sub(float x, double y) { return x - y; }
    public static Number sub(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(double x, byte y) { return x - y; }
    public static Number sub(double x, short y) { return x - y; }
    public static Number sub(double x, int y) { return x - y; }
    public static Number sub(double x, long y) { return x - y; }
    public static Number sub(double x, float y) { return x - y; }
    public static Number sub(double x, double y) { return x - y; }
    public static Number sub(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).subtract(y); }
    public static Number sub(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(BigInteger x, byte y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, short y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, int y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, long y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, float y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, double y) { return x.subtract(new BigInteger(String.valueOf(y))); }
    public static Number sub(BigInteger x, BigInteger y) { return x.subtract(y); }
    public static Number sub(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).subtract(y); }
    public static Number sub(BigDecimal x, byte y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, short y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, int y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, long y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, float y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, double y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, BigInteger y) { return x.subtract(new BigDecimal(String.valueOf(y))); }
    public static Number sub(BigDecimal x, BigDecimal y) { return x.subtract(y); }
    public static Number mul(byte x, byte y) { return x * y; }
    public static Number mul(byte x, short y) { return x * y; }
    public static Number mul(byte x, int y) { return x * y; }
    public static Number mul(byte x, long y) { return x * y; }
    public static Number mul(byte x, float y) { return x * y; }
    public static Number mul(byte x, double y) { return x * y; }
    public static Number mul(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(short x, byte y) { return x * y; }
    public static Number mul(short x, short y) { return x * y; }
    public static Number mul(short x, int y) { return x * y; }
    public static Number mul(short x, long y) { return x * y; }
    public static Number mul(short x, float y) { return x * y; }
    public static Number mul(short x, double y) { return x * y; }
    public static Number mul(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(int x, byte y) { return x * y; }
    public static Number mul(int x, short y) { return x * y; }
    public static Number mul(int x, int y) { return x * y; }
    public static Number mul(int x, long y) { return x * y; }
    public static Number mul(int x, float y) { return x * y; }
    public static Number mul(int x, double y) { return x * y; }
    public static Number mul(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(long x, byte y) { return x * y; }
    public static Number mul(long x, short y) { return x * y; }
    public static Number mul(long x, int y) { return x * y; }
    public static Number mul(long x, long y) { return x * y; }
    public static Number mul(long x, float y) { return x * y; }
    public static Number mul(long x, double y) { return x * y; }
    public static Number mul(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(float x, byte y) { return x * y; }
    public static Number mul(float x, short y) { return x * y; }
    public static Number mul(float x, int y) { return x * y; }
    public static Number mul(float x, long y) { return x * y; }
    public static Number mul(float x, float y) { return x * y; }
    public static Number mul(float x, double y) { return x * y; }
    public static Number mul(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(double x, byte y) { return x * y; }
    public static Number mul(double x, short y) { return x * y; }
    public static Number mul(double x, int y) { return x * y; }
    public static Number mul(double x, long y) { return x * y; }
    public static Number mul(double x, float y) { return x * y; }
    public static Number mul(double x, double y) { return x * y; }
    public static Number mul(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).multiply(y); }
    public static Number mul(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(BigInteger x, byte y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, short y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, int y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, long y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, float y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, double y) { return x.multiply(new BigInteger(String.valueOf(y))); }
    public static Number mul(BigInteger x, BigInteger y) { return x.multiply(y); }
    public static Number mul(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).multiply(y); }
    public static Number mul(BigDecimal x, byte y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, short y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, int y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, long y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, float y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, double y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, BigInteger y) { return x.multiply(new BigDecimal(String.valueOf(y))); }
    public static Number mul(BigDecimal x, BigDecimal y) { return x.multiply(y); }
    public static Number div(byte x, byte y) { return x / y; }
    public static Number div(byte x, short y) { return x / y; }
    public static Number div(byte x, int y) { return x / y; }
    public static Number div(byte x, long y) { return x / y; }
    public static Number div(byte x, float y) { return x / y; }
    public static Number div(byte x, double y) { return x / y; }
    public static Number div(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(short x, byte y) { return x / y; }
    public static Number div(short x, short y) { return x / y; }
    public static Number div(short x, int y) { return x / y; }
    public static Number div(short x, long y) { return x / y; }
    public static Number div(short x, float y) { return x / y; }
    public static Number div(short x, double y) { return x / y; }
    public static Number div(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(int x, byte y) { return x / y; }
    public static Number div(int x, short y) { return x / y; }
    public static Number div(int x, int y) { return x / y; }
    public static Number div(int x, long y) { return x / y; }
    public static Number div(int x, float y) { return x / y; }
    public static Number div(int x, double y) { return x / y; }
    public static Number div(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(long x, byte y) { return x / y; }
    public static Number div(long x, short y) { return x / y; }
    public static Number div(long x, int y) { return x / y; }
    public static Number div(long x, long y) { return x / y; }
    public static Number div(long x, float y) { return x / y; }
    public static Number div(long x, double y) { return x / y; }
    public static Number div(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(float x, byte y) { return x / y; }
    public static Number div(float x, short y) { return x / y; }
    public static Number div(float x, int y) { return x / y; }
    public static Number div(float x, long y) { return x / y; }
    public static Number div(float x, float y) { return x / y; }
    public static Number div(float x, double y) { return x / y; }
    public static Number div(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(double x, byte y) { return x / y; }
    public static Number div(double x, short y) { return x / y; }
    public static Number div(double x, int y) { return x / y; }
    public static Number div(double x, long y) { return x / y; }
    public static Number div(double x, float y) { return x / y; }
    public static Number div(double x, double y) { return x / y; }
    public static Number div(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).divide(y); }
    public static Number div(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(BigInteger x, byte y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, short y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, int y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, long y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, float y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, double y) { return x.divide(new BigInteger(String.valueOf(y))); }
    public static Number div(BigInteger x, BigInteger y) { return x.divide(y); }
    public static Number div(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).divide(y); }
    public static Number div(BigDecimal x, byte y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, short y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, int y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, long y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, float y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, double y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, BigInteger y) { return x.divide(new BigDecimal(String.valueOf(y))); }
    public static Number div(BigDecimal x, BigDecimal y) { return x.divide(y); }
    public static boolean eq(byte x, byte y) { return x == y; }
    public static boolean eq(byte x, short y) { return x == y; }
    public static boolean eq(byte x, int y) { return x == y; }
    public static boolean eq(byte x, long y) { return x == y; }
    public static boolean eq(byte x, float y) { return x == y; }
    public static boolean eq(byte x, double y) { return x == y; }
    public static boolean eq(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(short x, byte y) { return x == y; }
    public static boolean eq(short x, short y) { return x == y; }
    public static boolean eq(short x, int y) { return x == y; }
    public static boolean eq(short x, long y) { return x == y; }
    public static boolean eq(short x, float y) { return x == y; }
    public static boolean eq(short x, double y) { return x == y; }
    public static boolean eq(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(int x, byte y) { return x == y; }
    public static boolean eq(int x, short y) { return x == y; }
    public static boolean eq(int x, int y) { return x == y; }
    public static boolean eq(int x, long y) { return x == y; }
    public static boolean eq(int x, float y) { return x == y; }
    public static boolean eq(int x, double y) { return x == y; }
    public static boolean eq(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(long x, byte y) { return x == y; }
    public static boolean eq(long x, short y) { return x == y; }
    public static boolean eq(long x, int y) { return x == y; }
    public static boolean eq(long x, long y) { return x == y; }
    public static boolean eq(long x, float y) { return x == y; }
    public static boolean eq(long x, double y) { return x == y; }
    public static boolean eq(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(float x, byte y) { return x == y; }
    public static boolean eq(float x, short y) { return x == y; }
    public static boolean eq(float x, int y) { return x == y; }
    public static boolean eq(float x, long y) { return x == y; }
    public static boolean eq(float x, float y) { return x == y; }
    public static boolean eq(float x, double y) { return x == y; }
    public static boolean eq(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(double x, byte y) { return x == y; }
    public static boolean eq(double x, short y) { return x == y; }
    public static boolean eq(double x, int y) { return x == y; }
    public static boolean eq(double x, long y) { return x == y; }
    public static boolean eq(double x, float y) { return x == y; }
    public static boolean eq(double x, double y) { return x == y; }
    public static boolean eq(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).equals(y); }
    public static boolean eq(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(BigInteger x, byte y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, short y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, int y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, long y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, float y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, double y) { return x.equals(new BigInteger(String.valueOf(y))); }
    public static boolean eq(BigInteger x, BigInteger y) { return x.equals(y); }
    public static boolean eq(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).equals(y); }
    public static boolean eq(BigDecimal x, byte y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, short y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, int y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, long y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, float y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, double y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, BigInteger y) { return x.equals(new BigDecimal(String.valueOf(y))); }
    public static boolean eq(BigDecimal x, BigDecimal y) { return x.equals(y); }
    public static boolean lt(byte x, byte y) { return x < y; }
    public static boolean lt(byte x, short y) { return x < y; }
    public static boolean lt(byte x, int y) { return x < y; }
    public static boolean lt(byte x, long y) { return x < y; }
    public static boolean lt(byte x, float y) { return x < y; }
    public static boolean lt(byte x, double y) { return x < y; }
    public static boolean lt(byte x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(byte x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(short x, byte y) { return x < y; }
    public static boolean lt(short x, short y) { return x < y; }
    public static boolean lt(short x, int y) { return x < y; }
    public static boolean lt(short x, long y) { return x < y; }
    public static boolean lt(short x, float y) { return x < y; }
    public static boolean lt(short x, double y) { return x < y; }
    public static boolean lt(short x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(short x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(int x, byte y) { return x < y; }
    public static boolean lt(int x, short y) { return x < y; }
    public static boolean lt(int x, int y) { return x < y; }
    public static boolean lt(int x, long y) { return x < y; }
    public static boolean lt(int x, float y) { return x < y; }
    public static boolean lt(int x, double y) { return x < y; }
    public static boolean lt(int x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(int x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(long x, byte y) { return x < y; }
    public static boolean lt(long x, short y) { return x < y; }
    public static boolean lt(long x, int y) { return x < y; }
    public static boolean lt(long x, long y) { return x < y; }
    public static boolean lt(long x, float y) { return x < y; }
    public static boolean lt(long x, double y) { return x < y; }
    public static boolean lt(long x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(long x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(float x, byte y) { return x < y; }
    public static boolean lt(float x, short y) { return x < y; }
    public static boolean lt(float x, int y) { return x < y; }
    public static boolean lt(float x, long y) { return x < y; }
    public static boolean lt(float x, float y) { return x < y; }
    public static boolean lt(float x, double y) { return x < y; }
    public static boolean lt(float x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(float x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(double x, byte y) { return x < y; }
    public static boolean lt(double x, short y) { return x < y; }
    public static boolean lt(double x, int y) { return x < y; }
    public static boolean lt(double x, long y) { return x < y; }
    public static boolean lt(double x, float y) { return x < y; }
    public static boolean lt(double x, double y) { return x < y; }
    public static boolean lt(double x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(double x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(BigInteger x, byte y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, short y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, int y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, long y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, float y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, double y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) < 0; }
    public static boolean lt(BigInteger x, BigInteger y) { return x.compareTo(y) < 0; }
    public static boolean lt(BigInteger x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) < 0; }
    public static boolean lt(BigDecimal x, byte y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, short y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, int y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, long y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, float y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, double y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, BigInteger y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) < 0; }
    public static boolean lt(BigDecimal x, BigDecimal y) { return x.compareTo(y) < 0; }
    public static boolean gt(byte x, byte y) { return x > y; }
    public static boolean gt(byte x, short y) { return x > y; }
    public static boolean gt(byte x, int y) { return x > y; }
    public static boolean gt(byte x, long y) { return x > y; }
    public static boolean gt(byte x, float y) { return x > y; }
    public static boolean gt(byte x, double y) { return x > y; }
    public static boolean gt(byte x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(byte x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(short x, byte y) { return x > y; }
    public static boolean gt(short x, short y) { return x > y; }
    public static boolean gt(short x, int y) { return x > y; }
    public static boolean gt(short x, long y) { return x > y; }
    public static boolean gt(short x, float y) { return x > y; }
    public static boolean gt(short x, double y) { return x > y; }
    public static boolean gt(short x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(short x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(int x, byte y) { return x > y; }
    public static boolean gt(int x, short y) { return x > y; }
    public static boolean gt(int x, int y) { return x > y; }
    public static boolean gt(int x, long y) { return x > y; }
    public static boolean gt(int x, float y) { return x > y; }
    public static boolean gt(int x, double y) { return x > y; }
    public static boolean gt(int x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(int x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(long x, byte y) { return x > y; }
    public static boolean gt(long x, short y) { return x > y; }
    public static boolean gt(long x, int y) { return x > y; }
    public static boolean gt(long x, long y) { return x > y; }
    public static boolean gt(long x, float y) { return x > y; }
    public static boolean gt(long x, double y) { return x > y; }
    public static boolean gt(long x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(long x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(float x, byte y) { return x > y; }
    public static boolean gt(float x, short y) { return x > y; }
    public static boolean gt(float x, int y) { return x > y; }
    public static boolean gt(float x, long y) { return x > y; }
    public static boolean gt(float x, float y) { return x > y; }
    public static boolean gt(float x, double y) { return x > y; }
    public static boolean gt(float x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(float x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(double x, byte y) { return x > y; }
    public static boolean gt(double x, short y) { return x > y; }
    public static boolean gt(double x, int y) { return x > y; }
    public static boolean gt(double x, long y) { return x > y; }
    public static boolean gt(double x, float y) { return x > y; }
    public static boolean gt(double x, double y) { return x > y; }
    public static boolean gt(double x, BigInteger y) { return (new BigInteger(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(double x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(BigInteger x, byte y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, short y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, int y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, long y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, float y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, double y) { return (x.compareTo(new BigInteger(String.valueOf(y)))) > 0; }
    public static boolean gt(BigInteger x, BigInteger y) { return x.compareTo(y) > 0; }
    public static boolean gt(BigInteger x, BigDecimal y) { return (new BigDecimal(String.valueOf(x)).compareTo(y)) > 0; }
    public static boolean gt(BigDecimal x, byte y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, short y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, int y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, long y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, float y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, double y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, BigInteger y) { return (x.compareTo(new BigDecimal(String.valueOf(y)))) > 0; }
    public static boolean gt(BigDecimal x, BigDecimal y) { return x.compareTo(y) > 0; }
    public static Number modulo(byte x, byte y) { return x % y; }
    public static Number modulo(byte x, short y) { return x % y; }
    public static Number modulo(byte x, int y) { return x % y; }
    public static Number modulo(byte x, long y) { return x % y; }
    public static Number modulo(byte x, float y) { return x % y; }
    public static Number modulo(byte x, double y) { return x % y; }
    public static Number modulo(byte x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(byte x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(short x, byte y) { return x % y; }
    public static Number modulo(short x, short y) { return x % y; }
    public static Number modulo(short x, int y) { return x % y; }
    public static Number modulo(short x, long y) { return x % y; }
    public static Number modulo(short x, float y) { return x % y; }
    public static Number modulo(short x, double y) { return x % y; }
    public static Number modulo(short x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(short x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(int x, byte y) { return x % y; }
    public static Number modulo(int x, short y) { return x % y; }
    public static Number modulo(int x, int y) { return x % y; }
    public static Number modulo(int x, long y) { return x % y; }
    public static Number modulo(int x, float y) { return x % y; }
    public static Number modulo(int x, double y) { return x % y; }
    public static Number modulo(int x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(int x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(long x, byte y) { return x % y; }
    public static Number modulo(long x, short y) { return x % y; }
    public static Number modulo(long x, int y) { return x % y; }
    public static Number modulo(long x, long y) { return x % y; }
    public static Number modulo(long x, float y) { return x % y; }
    public static Number modulo(long x, double y) { return x % y; }
    public static Number modulo(long x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(long x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(float x, byte y) { return x % y; }
    public static Number modulo(float x, short y) { return x % y; }
    public static Number modulo(float x, int y) { return x % y; }
    public static Number modulo(float x, long y) { return x % y; }
    public static Number modulo(float x, float y) { return x % y; }
    public static Number modulo(float x, double y) { return x % y; }
    public static Number modulo(float x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(float x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(double x, byte y) { return x % y; }
    public static Number modulo(double x, short y) { return x % y; }
    public static Number modulo(double x, int y) { return x % y; }
    public static Number modulo(double x, long y) { return x % y; }
    public static Number modulo(double x, float y) { return x % y; }
    public static Number modulo(double x, double y) { return x % y; }
    public static Number modulo(double x, BigInteger y) { return new BigInteger(String.valueOf(x)).remainder(y); }
    public static Number modulo(double x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(BigInteger x, byte y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, short y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, int y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, long y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, float y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, double y) { return x.remainder(new BigInteger(String.valueOf(y))); }
    public static Number modulo(BigInteger x, BigInteger y) { return x.remainder(y); }
    public static Number modulo(BigInteger x, BigDecimal y) { return new BigDecimal(String.valueOf(x)).remainder(y); }
    public static Number modulo(BigDecimal x, byte y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, short y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, int y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, long y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, float y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, double y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, BigInteger y) { return x.remainder(new BigDecimal(String.valueOf(y))); }
    public static Number modulo(BigDecimal x, BigDecimal y) { return x.remainder(y); }
}
