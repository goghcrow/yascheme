package xiao.lang;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.Character.isDigit;
import static java.lang.Character.isWhitespace;
import static xiao.lang.Contract.expect;
import static xiao.lang.Misc.Nullable;
import static xiao.lang.Values.*;
import static xiao.lang.Values.Symbol;

/**
 * @author chuxiaofeng
 */
public class Parser {

    public static List<Object> parse(String s) {
        return new Parser(s).parse();
    }

    public static Object parse1(String s) {
        List<Object> ns = new Parser(s).parse();
        expect(ns.size() == 1, "期望 size == 1, 实际" + s);
        return ns.get(0);
    }



    final static String LINE_COMMENT = ";";

    final static String SYNTAX = "#'";
    final static String QUASI_SYNTAX = "#`";
    final static String SHARP_TUPLE_BEGIN = "#(";
    final static String SHARP_VECTOR_BEGIN = "#[";
    final static String TUPLE_BEGIN = "(";
    final static String TUPLE_END = ")";
    final static String VECTOR_BEGIN = "[";
    final static String VECTOR_END = "]";

    final static String CHAR = "#\\";
    final static String QUOTE = "'";
    final static String UNQUOTE = ",";
    final static String UNQUOTE_SPLICING = ",@";
    final static String QUASIQUOTE = "`";

    final static Map<String, Character> charNames = new HashMap<>();
    static {
        charNames.put("nul", (char) Integer.parseInt("0000", 16));
        charNames.put("alarm", (char) Integer.parseInt("0007", 16));
        charNames.put("backspace", (char) Integer.parseInt("0008", 16));
        charNames.put("tab", (char) Integer.parseInt("0009", 16));
        charNames.put("linefeed", (char) Integer.parseInt("000A", 16));
        charNames.put("newline", (char) Integer.parseInt("000A", 16));
        charNames.put("vtab", (char) Integer.parseInt("000B", 16));
        charNames.put("page", (char) Integer.parseInt("000C", 16));
        charNames.put("return", (char) Integer.parseInt("000D", 16));
        charNames.put("esc", (char) Integer.parseInt("001B", 16));
        charNames.put("space", (char) Integer.parseInt("0020", 16));
        charNames.put("delete", (char) Integer.parseInt("007F", 16));
    }



    final Pattern charPattern;

    final Set<String> delimiters = new HashSet<>();
    final Map<String, String> delimiterMap = new HashMap<>();

    final String input;
    int offset = 0;

    public Parser(String input) {
        this.input = input;

        addDelimiterPair(SHARP_TUPLE_BEGIN, TUPLE_END);
        addDelimiterPair(SHARP_VECTOR_BEGIN, VECTOR_END);
        addDelimiterPair(TUPLE_BEGIN, TUPLE_END);
        addDelimiterPair(VECTOR_BEGIN, VECTOR_END);

        addDelimiter(SYNTAX);
        addDelimiter(CHAR);
        addDelimiter(QUOTE);
        addDelimiter(UNQUOTE);
        addDelimiter(UNQUOTE_SPLICING);
        addDelimiter(QUASIQUOTE);

        charPattern = buildCharPattern();
    }

    Pattern buildCharPattern() {
        StringJoiner cns = new StringJoiner(")|(?:", "(?:", ")");
        for (String cn : charNames.keySet()) {
            cns.add(cn);
        }
        StringJoiner delis = new StringJoiner(")|(", "$|\\s|\\r|\\n|" + LINE_COMMENT + "|(?:", ")");
        for (String del : delimiters) {
            delis.add(java.util.regex.Pattern.quote(del));
        }
        //noinspection RegExpUnnecessaryNonCapturingGroup
        return Pattern.compile("((?:" + cns + ")|(?:x[a-fA-F0-9]+)|.)(?=" + delis + ")");
    }

    void addDelimiterPair(String open, String close) {
        delimiters.add(open);
        delimiters.add(close);
        delimiterMap.put(open, close);
    }

    void addDelimiter(String delim) {
        delimiters.add(delim);
    }

    @Nullable String isDelimiter() {
        String delimiter = null;
        for (String del : delimiters) {
            if (input.startsWith(del, offset)) {
                if (delimiter == null) {
                    delimiter = del;
                } else {
                    if (del.length() > delimiter.length()) {
                        delimiter = del;
                    }
                }
            }
        }
        return delimiter;
    }

    boolean isDelimiter(char c) {
        return delimiters.contains(Character.toString(c));
    }

    boolean isOpen(Object c) {
        if (c instanceof Delimiter) {
            return delimiterMap.containsKey(((Delimiter) c).shape);
        } else {
            return false;
        }
    }

    boolean isClose(Object c) {
        if (c instanceof Delimiter) {
            return delimiterMap.containsValue(((Delimiter) c).shape);
        } else {
            return false;
        }
    }

    boolean matchDelimiter(Object open, Object close) {
        return (open instanceof Delimiter &&
                close instanceof Delimiter &&
                matchString(
                        ((Delimiter) open).shape,
                        ((Delimiter) close).shape
                )
        );
    }

    boolean matchString(String open, String close) {
        String matched = delimiterMap.get(open);
        return matched != null && matched.equals(close);
    }

    @Nullable
    Symbol isSyntax(Object n) {
        if (n instanceof Delimiter) {
            if (((Delimiter) n).shape.equals(SYNTAX)) {
                return RT.sym(Names.SYNTAX);
            }
        }
        return null;
    }

    @Nullable
    Symbol isQuotation(Object n) {
        if (n instanceof Delimiter) {
            String s = ((Delimiter) n).shape;
            switch (s) {
                case QUOTE:
                    return RT.sym(Names.QUOTE);
                case QUASIQUOTE:
                    return RT.sym(Names.QUASIQUOTE);
                case UNQUOTE:
                    return RT.sym(Names.UNQUOTE);
                case UNQUOTE_SPLICING:
                    return RT.sym(Names.UNQUOTE_SPLICING);
            }
        }
        return null;
    }

    boolean isCharPrefix(Object n) {
        if (n instanceof Delimiter) {
            String s = ((Delimiter) n).shape;
            return s.equals(CHAR);
        }
        return false;
    }

    public List<Object> parse() {
        List<Object> els = new ArrayList<>();
        Object s = nextSexp();
        while (s != null) {
            els.add(s);
            s = nextSexp();
        }
        return els;
    }

    Object nextSexp() {
        return nextNode(0);
    }

    @Nullable
    Object nextNode(int depth) {
        Object tok = nextToken();
        if (tok == null) {
            return null;
        }

        Symbol quote;
        if (depth == 0 && isClose(tok)) {
            throw new InterpError("不匹配: " + tok);
        } else if (isOpen(tok)) {
            List<Object> els = new ArrayList<>();
            Object iter = nextNode(depth + 1);
            while (!matchDelimiter(tok, iter)) {
                if (iter == null) {
                    throw new InterpError("未闭合: " + tok);
                } else if (isClose(iter)) {
                    throw new InterpError("不匹配: " + iter);
                } else {
                    els.add(iter);
                    iter = nextNode(depth + 1);
                }
            }
            String del = ((Delimiter) tok).shape;
            if (del.equals(SHARP_TUPLE_BEGIN) || del.equals(SHARP_VECTOR_BEGIN)) {
                // vector 字面量
                return els.toArray();
            } else {
                return RT.listColl(els);
            }
        } else if ((quote = isQuotation(tok)) != null) {
            return RT.list(quote, nextNode(depth));
        } else if ((quote = isSyntax(tok)) != null) {
            return RT.list(quote, nextNode(depth));
        } else if (isCharPrefix(tok)) {
            return readChar();
        } else {
            return tok;
        }
    }

    char readChar() {
        Matcher matcher = charPattern.matcher(input.substring(offset));
        expect(matcher.lookingAt(), "错误的 char 格式");
        String s = input.substring(offset, matcher.end() + offset);
        offset += matcher.end();
        if (s.length() == 1) {
            return s.charAt(0);
        } else if (charNames.containsKey(s)) {
            return charNames.get(s);
        } else if (s.startsWith("x")) {
            return (char) Integer.parseInt(s.substring(1), 16);
        } else {
            throw new InterpError("错误的 char 格式");
        }
    }

    @Nullable
    Object nextToken() {
        skipComment();
        if (offset >= input.length()) {
            return null;
        }

        String del = isDelimiter();
        if (del != null) {
            offset += del.length();
            return new Delimiter(del);
        }

        if (offset >= input.length()) {
            return null;
        }

        char cur = input.charAt(offset);
        if (input.charAt(offset) == '"' && (offset == 0 || input.charAt(offset - 1) != '\\')) {
            int start = offset;
            offset++; // skip "
            while (offset < input.length()) {
                if (input.charAt(offset) == '"') {
                    if (input.charAt(offset - 1) == '\\') {
                        // "\\"
                        if ((offset - 2 >= 0 && input.charAt(offset - 2) == '\\')) {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if (input.charAt(offset) == '\n') {
                    throw new InterpError("字符串不能换行");
                }
                offset++;
            }
            if (offset >= input.length()) {
                throw new InterpError("未闭合字符串");
            }
            offset++; // skip "
            int end = offset;
            String content = input.substring(start/* + 1*/, end/* - 1*/);
            // String-Value
            return UnEscapeStr.unescape(content);
        }

        int start = offset;
        if (isDigit(input.charAt(start)) ||
                ((input.charAt(start) == '+' || input.charAt(start) == '-') && isDigit(input.charAt(start + 1)))) {
            while (offset < input.length() && !isWhitespace(cur) && !(isDelimiter(cur) && cur != '.')) {
                if (++offset < input.length()) {
                    cur = input.charAt(offset);
                }
            }

            boolean mayBeHex = input.startsWith("+0x", start)
                    || input.startsWith("0x", start)
                    || input.startsWith("-0x", start);

            char lc = input.charAt(offset - 1);
            if (!mayBeHex && (lc == 'b' || lc == 'B')) {
                String content = input.substring(start, offset - 1);
                try {
                    // Byte-Value
                    return Byte.parseByte(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 byte 格式: " + content);
                }
            } else if (lc == 's' || lc == 'S') {
                String content = input.substring(start, offset - 1);
                try {
                    // Short-Value
                    return Short.parseShort(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 short 格式: " + content);
                }
            } else if (lc == 'l' || lc == 'L') {
                String content = input.substring(start, offset - 1);
                try {
                    // Long-Value
                    return parseNum(content, Long.class);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 long 格式: " + content);
                }
            } else if (!mayBeHex && (lc == 'f' || lc == 'F')) {
                String content = input.substring(start, offset - 1);
                try {
                    // Float-Value
                    return Float.parseFloat(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 float 格式: " + content);
                }
            } else if (!mayBeHex && (lc == 'd' || lc == 'D')) {
                String content = input.substring(start, offset - 1);
                try {
                    // Double-Value
                    return Double.parseDouble(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 double 格式: " + content);
                }
            } else if (lc == 'n' || lc == 'N') {
                String content = input.substring(start, offset - 1);
                try {
                    // BigInt-Value
                    return new BigInteger(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 bigint 格式: " + content);
                }
            } else if (lc == 'm' || lc == 'M') {
                String content = input.substring(start, offset - 1);
                try {
                    // BigDec-Value
                    return new BigDecimal(content);
                } catch (NumberFormatException e) {
                    throw new InterpError("错误的 bigdec 格式: " + content);
                }
            } else {
                String content = input.substring(start, offset);
                // 这里 Integer 溢出会用 Double 表示
                try {
                    // Int-Value
                    return parseNum(content, Integer.class);
                } catch (NumberFormatException e1) {
                    try {
                        // Double-Value
                        return Double.parseDouble(content);
                    } catch (NumberFormatException e) {
                        throw new InterpError("错误的数字格式: " + content);
                    }
                }
            }
        }

        while (offset < input.length() && !isWhitespace(cur) && !isDelimiter(cur)) {
            if (++offset < input.length()) {
                cur = input.charAt(offset);
            }
        }

        String name = input.substring(start, offset);
        // 1. 模式匹配需要把 bool 识别成字面量
        // 2. 健康宏展开需要把 #t #f 处理成 #%datum, 所以这里把布尔#t,#f 从 name 改成 primitive
        if (Names.TRUE.equals(name)) {
            return true; // Bool-Value
        } else if (Names.FALSE.equals(name)) {
            return false; // Bool-Value
        } else {
            if (name.startsWith(Names.KEYWORD_PREFIX)) {
                return RT.keyword(name); // Keyword-Value
            } else {
                return RT.sym(name); // Symbol-Value
            }
        }
    }

    void skipComment() {
        boolean seenComment = true;
        while (seenComment) {
            seenComment = false;
            while (offset < input.length() && isWhitespace(input.charAt(offset))) {
                offset++;
            }
            if (offset + LINE_COMMENT.length() <= input.length() && input.startsWith(LINE_COMMENT, offset)) {
                while (offset < input.length() && input.charAt(offset) != '\n') {
                    offset++;
                }
                if (offset < input.length()) {
                    offset++;
                }
                seenComment = true;
            }
        }
    }

    Number parseNum(String content, Class<?> type) {
        int sign;
        if (content.startsWith("+")) {
            sign = 1;
            content = content.substring(1);
        } else if (content.startsWith("-")) {
            sign = -1;
            content = content.substring(1);
        } else {
            sign = 1;
        }

        int base;
        if (content.startsWith("0b")) {
            base = 2;
            content = content.substring(2);
        } else if (content.startsWith("0x")) {
            base = 16;
            content = content.substring(2);
        } else if (content.startsWith("0o")) {
            base = 8;
            content = content.substring(2);
        } else {
            base = 10;
        }

        if (type == Integer.class) {
            int val = Integer.parseInt(content, base);
            return sign == -1 ? -val : val;
        } else if (type == Long.class) {
            long val = Long.parseLong(content, base);
            return sign == -1 ? -val : val;
        } else {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * @author chuxiaofeng
     */
    static class UnEscapeStr {
        static String unescape(String s) {
            char quote = s.charAt(0);
            s = s.substring(1, s.length() - 1);

            char[] a = s.toCharArray(), ss = new char[a.length];
            int l = a.length, cnt = 0;

            for (int i = 0; i < l; i++) {
                char c = a[i];
                if (c == quote && i + 1 < l) {
                    // """"   ''''
                    char n = a[i + 1];
                    if (n == quote) {
                        i++;
                        ss[cnt++] = quote;
                    } else {
                        ss[cnt++] = c;
                    }
                } else if (c == '\\' && i + 1 < l) {
                    // \' \" \\ \t \r \n \b \f
                    char n = a[i + 1];
                    i++;
                    if (n == quote) {
                        ss[cnt++] = quote;
                    } else {
                        switch (n) {
                            // case quote: ss[cnt++] = quote ;break;
                            case '\\': ss[cnt++] = '\\';break;
                            // case '/': ss[cnt++] = '/';break;
                            case 't': ss[cnt++] = '\t';break;
                            case 'r': ss[cnt++] = '\r';break;
                            case 'n': ss[cnt++] = '\n';break;
                            case 'b': ss[cnt++] = '\b';break;
                            case 'f': ss[cnt++] = '\f';break;
                            case 'u':
                                expect(i + 4 < a.length);
                                ss[cnt++] = parseUnicodeEscape(a[i + 1], a[i + 2], a[i + 3], a[i + 4]);
                                i += 4;
                                break;
                            default:
                                i--;
                                ss[cnt++] = c;
                        }
                    }
                } else {
                    ss[cnt++] = c;
                }
            }
            return new String(ss, 0, cnt);
        }

        static char parseUnicodeEscape(char c1, char c2, char c3, char c4) {
            // return (char) Integer.parseInt(String.valueOf(c1) + c2 + c3 + c4, 16);
            int i = parseHexDigit(c1) << 12 | parseHexDigit(c2) << 8 | parseHexDigit(c3) << 4 | parseHexDigit(c4);
            if (Double.isInfinite(i) || Double.isNaN(i)) {
                throw new InterpError("有问题的\\u Unicode");
            }
            return (char) i;
        }

        static int parseHexDigit(char c) {
            if (c >= '0' && c <= '9') {
                return c - '0';
            } else if (c >= 'A' && c <= 'F') {
                return c + 10 - 'A';
            } else if (c >= 'a' && c <= 'f') {
                return c + 10 - 'a';
            }
            throw new InterpError("有问题的\\u Unicode");
        }
    }


    static class Delimiter {
        final String shape;

        Delimiter(String shape) {
            this.shape = shape;
        }

        @Override
        public String toString() {
            return shape;
        }
    }
}
