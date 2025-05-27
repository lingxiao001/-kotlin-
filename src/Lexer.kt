import java.io.File


// Position.kt - 位置信息
data class Position(
    val line: Int,
    val column: Int,
    val offset: Int
) {
    override fun toString(): String = "($line:$column)"
}

// TokenType.kt - Token类型枚举
enum class TokenType(val category: String) {
    // 关键字
    AUTO("KEYWORD"), BREAK("KEYWORD"), CASE("KEYWORD"), CHAR("KEYWORD"),
    CONST("KEYWORD"), CONTINUE("KEYWORD"), DEFAULT("KEYWORD"), DO("KEYWORD"),
    DOUBLE("KEYWORD"), ELSE("KEYWORD"), ENUM("KEYWORD"), EXTERN("KEYWORD"),
    FLOAT("KEYWORD"), FOR("KEYWORD"), GOTO("KEYWORD"), IF("KEYWORD"),
    INT("KEYWORD"), LONG("KEYWORD"), REGISTER("KEYWORD"), RETURN("KEYWORD"),
    SHORT("KEYWORD"), SIGNED("KEYWORD"), SIZEOF("KEYWORD"), STATIC("KEYWORD"),
    STRUCT("KEYWORD"), SWITCH("KEYWORD"), TYPEDEF("KEYWORD"), UNION("KEYWORD"),
    UNSIGNED("KEYWORD"), VOID("KEYWORD"), VOLATILE("KEYWORD"), WHILE("KEYWORD"),
    INCLUDE("KEYWORD"),

    // 标识符和字面量
    IDENTIFIER("IDENTIFIER"),
    INTEGER_LITERAL("LITERAL"),
    FLOAT_LITERAL("LITERAL"),
    CHAR_LITERAL("LITERAL"),
    STRING_LITERAL("LITERAL"),

    // 运算符
    PLUS("OPERATOR"),           // +
    MINUS("OPERATOR"),          // -
    MULTIPLY("OPERATOR"),       // *
    DIVIDE("OPERATOR"),         // /
    MODULO("OPERATOR"),         // %
    ASSIGN("OPERATOR"),         // =
    PLUS_ASSIGN("OPERATOR"),    // +=
    MINUS_ASSIGN("OPERATOR"),   // -=
    MULTIPLY_ASSIGN("OPERATOR"), // *=
    DIVIDE_ASSIGN("OPERATOR"),  // /=
    MODULO_ASSIGN("OPERATOR"),  // %=
    INCREMENT("OPERATOR"),      // ++
    DECREMENT("OPERATOR"),      // --

    // 比较运算符
    EQUAL("OPERATOR"),          // ==
    NOT_EQUAL("OPERATOR"),      // !=
    LESS_THAN("OPERATOR"),      // <
    GREATER_THAN("OPERATOR"),   // >
    LESS_EQUAL("OPERATOR"),     // <=
    GREATER_EQUAL("OPERATOR"),  // >=

    // 逻辑运算符
    LOGICAL_AND("OPERATOR"),    // &&
    LOGICAL_OR("OPERATOR"),     // ||
    LOGICAL_NOT("OPERATOR"),    // !

    // 位运算符
    BITWISE_AND("OPERATOR"),    // &
    BITWISE_OR("OPERATOR"),     // |
    BITWISE_XOR("OPERATOR"),    // ^
    BITWISE_NOT("OPERATOR"),    // ~
    LEFT_SHIFT("OPERATOR"),     // <<
    RIGHT_SHIFT("OPERATOR"),    // >>

    // 分隔符
    LEFT_PAREN("DELIMITER"),    // (
    RIGHT_PAREN("DELIMITER"),   // )
    LEFT_BRACE("DELIMITER"),    // {
    RIGHT_BRACE("DELIMITER"),   // }
    LEFT_BRACKET("DELIMITER"),  // [
    RIGHT_BRACKET("DELIMITER"), // ]
    SEMICOLON("DELIMITER"),     // ;
    COMMA("DELIMITER"),         // ,
    DOT("DELIMITER"),           // .
    ARROW("DELIMITER"),         // ->
    QUESTION("DELIMITER"),      // ?
    COLON("DELIMITER"),         // :

    // 预处理器
    HASH("PREPROCESSOR"),       // #

    // 特殊
    NEWLINE("SPECIAL"),
    EOF("SPECIAL"),
    COMMENT("COMMENT"),
    UNKNOWN("UNKNOWN");

    companion object {
        private val keywords = mapOf(
            "auto" to AUTO, "break" to BREAK, "case" to CASE, "char" to CHAR,
            "const" to CONST, "continue" to CONTINUE, "default" to DEFAULT, "do" to DO,
            "double" to DOUBLE, "else" to ELSE, "enum" to ENUM, "extern" to EXTERN,
            "float" to FLOAT, "for" to FOR, "goto" to GOTO, "if" to IF,
            "int" to INT, "long" to LONG, "register" to REGISTER, "return" to RETURN,
            "short" to SHORT, "signed" to SIGNED, "sizeof" to SIZEOF, "static" to STATIC,
            "struct" to STRUCT, "switch" to SWITCH, "typedef" to TYPEDEF, "union" to UNION,
            "unsigned" to UNSIGNED, "void" to VOID, "volatile" to VOLATILE, "while" to WHILE,
            "include" to INCLUDE
        )

        fun getKeyword(identifier: String): TokenType? = keywords[identifier] // identifier标识符映射为TokenType 并return TokenType
    }
}

// Token.kt - Token数据类
data class Token(
    val type: TokenType,
    val lexeme: String, //源代码的原始文本
    val position: Position,
    val value: Any? = null //token字面量值 对于num String存在
) {
    fun toOutputString(): String {
        return "${type.category}\t${type.name}\t$lexeme"  //token的字符串表示！
    }
}

// LexerException.kt - 词法分析异常
class LexerException(
    message: String,
    val position: Position
) : Exception("Lexer error at ${position}: $message")

// Lexer.kt - 词法分析器主类
class CLexer(private val source: String) {
    private var current = 0
    private var line = 1
    private var column = 1
    private val tokens = mutableListOf<Token>()

    //主函数 -- 驱动语法分析过程
    fun tokenize(): List<Token> {
        while (!isAtEnd()) {
            scanToken()
        }

        tokens.add(Token(TokenType.EOF, "", currentPosition()))
        return tokens
    }
    //处理单个token
    private fun scanToken() {
        val start = current
        val c = advance()

        when (c) {
            // 空白字符
            ' ', '\r', '\t' -> {
                // 忽略空白字符
            }
            '\n' -> {
                tokens.add(Token(TokenType.NEWLINE, "\\n", positionAt(start)))
                line++
                column = 1
            }

            // 单字符token
            '(' -> addToken(TokenType.LEFT_PAREN, start)
            ')' -> addToken(TokenType.RIGHT_PAREN, start)
            '{' -> addToken(TokenType.LEFT_BRACE, start)
            '}' -> addToken(TokenType.RIGHT_BRACE, start)
            '[' -> addToken(TokenType.LEFT_BRACKET, start)
            ']' -> addToken(TokenType.RIGHT_BRACKET, start)
            ',' -> addToken(TokenType.COMMA, start)
            '.' -> addToken(TokenType.DOT, start)
            ';' -> addToken(TokenType.SEMICOLON, start)
            '~' -> addToken(TokenType.BITWISE_NOT, start)
            '?' -> addToken(TokenType.QUESTION, start)
            ':' -> addToken(TokenType.COLON, start)
            '#' -> addToken(TokenType.HASH, start)

            // 可能是双字符的操作符
            '+' -> {
                when {
                    match('+') -> addToken(TokenType.INCREMENT, start)
                    match('=') -> addToken(TokenType.PLUS_ASSIGN, start)
                    else -> addToken(TokenType.PLUS, start)
                }
            }
            '-' -> {
                when {
                    match('-') -> addToken(TokenType.DECREMENT, start)
                    match('=') -> addToken(TokenType.MINUS_ASSIGN, start)
                    match('>') -> addToken(TokenType.ARROW, start)
                    else -> addToken(TokenType.MINUS, start)
                }
            }
            '*' -> {
                if (match('=')) addToken(TokenType.MULTIPLY_ASSIGN, start)
                else addToken(TokenType.MULTIPLY, start)
            }
            '/' -> {
                when {
                    match('=') -> addToken(TokenType.DIVIDE_ASSIGN, start)
                    match('/') -> {
                        // 单行注释
                        while (peek() != '\n' && !isAtEnd()) advance()
                        val comment = source.substring(start, current)
                        addToken(TokenType.COMMENT, start, comment)
                    }
                    match('*') -> {
                        // 多行注释
                        blockComment(start)
                    }
                    else -> addToken(TokenType.DIVIDE, start)
                }
            }
            '%' -> {
                if (match('=')) addToken(TokenType.MODULO_ASSIGN, start)
                else addToken(TokenType.MODULO, start)
            }
            '=' -> {
                if (match('=')) addToken(TokenType.EQUAL, start)
                else addToken(TokenType.ASSIGN, start)
            }
            '!' -> {
                if (match('=')) addToken(TokenType.NOT_EQUAL, start)
                else addToken(TokenType.LOGICAL_NOT, start)
            }
            '<' -> {
                when {
                    match('=') -> addToken(TokenType.LESS_EQUAL, start)
                    match('<') -> addToken(TokenType.LEFT_SHIFT, start)
                    else -> addToken(TokenType.LESS_THAN, start)
                }
            }
            '>' -> {
                when {
                    match('=') -> addToken(TokenType.GREATER_EQUAL, start)
                    match('>') -> addToken(TokenType.RIGHT_SHIFT, start)
                    else -> addToken(TokenType.GREATER_THAN, start)
                }
            }
            '&' -> {
                if (match('&')) addToken(TokenType.LOGICAL_AND, start)
                else addToken(TokenType.BITWISE_AND, start)
            }
            '|' -> {
                if (match('|')) addToken(TokenType.LOGICAL_OR, start)
                else addToken(TokenType.BITWISE_OR, start)
            }
            '^' -> addToken(TokenType.BITWISE_XOR, start)

            // 字符串字面量
            '"' -> string(start)

            // 字符字面量
            '\'' -> character(start)

            else -> {
                when {
                    c.isDigit() -> number(start)
                    c.isLetter() || c == '_' -> identifier(start)
                    else -> throw LexerException("Unexpected character: $c", positionAt(start))
                }
            }
        }
    }
    //处理多行注释
    private fun blockComment(start: Int) {
        while (!isAtEnd()) {
            if (peek() == '*' && peekNext() == '/') {
                advance() // consume '*'
                advance() // consume '/'
                break
            }
            if (peek() == '\n') {
                line++
                column = 1
            }
            advance()
        }
        val comment = source.substring(start, current)
        addToken(TokenType.COMMENT, start, comment)
    }

    private fun string(start: Int) {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') {
                line++
                column = 1
            }
            if (peek() == '\\') advance() // 跳过转义字符
            advance()
        }

        if (isAtEnd()) {
            throw LexerException("Unterminated string", positionAt(start))
        }

        advance() // 消费结束的 "
        val value = source.substring(start + 1, current - 1)
        addToken(TokenType.STRING_LITERAL, start, value)
    }

    private fun character(start: Int) {
        if (peek() == '\\') advance() // 处理转义字符
        if (!isAtEnd()) advance() // 字符内容

        if (peek() != '\'' || isAtEnd()) {
            throw LexerException("Unterminated character literal", positionAt(start))
        }

        advance() // 消费结束的 '
        val value = source.substring(start + 1, current - 1)
        addToken(TokenType.CHAR_LITERAL, start, value)
    }

    private fun number(start: Int) {
        // 整数部分
        while (peek().isDigit()) advance()

        // 浮点数
        if (peek() == '.' && peekNext().isDigit()) {
            advance() // 消费 '.'
            while (peek().isDigit()) advance()

            // 科学计数法
            if (peek() == 'e' || peek() == 'E') {
                advance()
                if (peek() == '+' || peek() == '-') advance()
                while (peek().isDigit()) advance()
            }

            val value = source.substring(start, current).toDoubleOrNull()
            addToken(TokenType.FLOAT_LITERAL, start, value)
        } else {
            // 整数后缀 (L, U, etc.)
            if (peek() == 'L' || peek() == 'l' || peek() == 'U' || peek() == 'u') {
                advance()
                if (peek() == 'L' || peek() == 'l' || peek() == 'U' || peek() == 'u') {
                    advance()
                }
            }

            val lexeme = source.substring(start, current)
            val value = lexeme.replace(Regex("[LlUu]"), "").toLongOrNull()
            addToken(TokenType.INTEGER_LITERAL, start, value)
        }
    }

    private fun identifier(start: Int) {
        while (peek().isLetterOrDigit() || peek() == '_') advance()

        val text = source.substring(start, current)
        val type = TokenType.getKeyword(text) ?: TokenType.IDENTIFIER
        addToken(type, start)
    }

    private fun match(expected: Char): Boolean {
        if (isAtEnd()) return false
        if (source[current] != expected) return false

        current++
        column++
        return true
    }

    private fun peek(): Char {
        if (isAtEnd()) return '\u0000'
        return source[current]
    }

    private fun peekNext(): Char {
        if (current + 1 >= source.length) return '\u0000'
        return source[current + 1]
    }
    //读取当前字符并移动指针
    private fun advance(): Char {
        val char = source[current++]
        column++
        return char
    }

    private fun isAtEnd(): Boolean = current >= source.length

    //添加一个token到tokens列表
    private fun addToken(type: TokenType, start: Int, value: Any? = null) {
        val text = source.substring(start, current)
        tokens.add(Token(type, text, positionAt(start), value))
    }
    //根据偏移量offset计算位置
    private fun positionAt(offset: Int): Position {
        var line = 1
        var column = 1
        for (i in 0 until offset) {
            if (source[i] == '\n') {
                line++
                column = 1
            } else {
                column++
            }
        }
        return Position(line, column, offset)
    }

    private fun currentPosition(): Position = Position(line, column, current)
}

// CompilerMain.kt - 主程序


fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("Usage: kotlin CompilerMain <source-file>")
        return
    }

    val sourceFile = File(args[0])
    if (!sourceFile.exists()) {
        println("Error: File ${args[0]} not found")
        return
    }

    try {
        val sourceCode = sourceFile.readText()
        val lexer = CLexer(sourceCode)
        val tokens = lexer.tokenize()

        // 输出到控制台 (三列格式)
        println("=== Lexical Analysis Results ===")
        println("CATEGORY\t\tTOKEN_TYPE\t\tLEXEME")
        println("=" .repeat(60))

        for (token in tokens) {
            if (token.type != TokenType.NEWLINE && token.type != TokenType.EOF) {
                println(token.toOutputString())
            }
        }

        // 生成中间文件
        val outputFile = File(sourceFile.nameWithoutExtension + "_tokens.txt")
        outputFile.writeText(generateTokenFile(tokens))
        println("\nToken file generated: ${outputFile.name}")

        // 生成符号表文件 (用于语法分析)
        val symbolFile = File(sourceFile.nameWithoutExtension + "_symbols.txt")
        symbolFile.writeText(generateSymbolFile(tokens))
        println("Symbol file generated: ${symbolFile.name}")

        // 统计信息
        printStatistics(tokens)

    } catch (e: LexerException) {
        println("Lexical Error: ${e.message}")
    } catch (e: Exception) {
        println("Error: ${e.message}")
    }
}

fun generateTokenFile(tokens: List<Token>): String {
    val sb = StringBuilder()
    sb.appendLine("# Token File for Parser")
    sb.appendLine("# Format: TOKEN_TYPE|LEXEME|LINE:COLUMN")
    sb.appendLine()

    for (token in tokens) {
        if (token.type != TokenType.NEWLINE) {
            sb.appendLine("${token.type.name}|${token.lexeme}|${token.position}")
        }
    }
    return sb.toString()
}

fun generateSymbolFile(tokens: List<Token>): String {
    val sb = StringBuilder()
    sb.appendLine("# Symbol Table")
    sb.appendLine("# IDENTIFIER_NAME|FIRST_OCCURRENCE_LINE|COUNT")
    sb.appendLine()

    val identifiers = mutableMapOf<String, Pair<Int, Int>>()

    for (token in tokens) {
        if (token.type == TokenType.IDENTIFIER) {
            val current = identifiers[token.lexeme]
            if (current == null) {
                identifiers[token.lexeme] = Pair(token.position.line, 1)
            } else {
                identifiers[token.lexeme] = Pair(current.first, current.second + 1)
            }
        }
    }

    for ((name, info) in identifiers.toSortedMap()) {
        sb.appendLine("$name|${info.first}|${info.second}")
    }

    return sb.toString()
}

fun printStatistics(tokens: List<Token>) {
    println("\n=== Statistics ===")
    val stats = tokens.groupBy { it.type.category }

    for ((category, tokenList) in stats) {
        println("$category: ${tokenList.size}")
    }

    println("Total tokens: ${tokens.size}")

    val identifierCount = tokens.count { it.type == TokenType.IDENTIFIER }
    val uniqueIdentifierCount = tokens.filter { it.type == TokenType.IDENTIFIER }
        .map { it.lexeme }.distinct().size

    println("Identifiers: $identifierCount (unique: $uniqueIdentifierCount)")
}