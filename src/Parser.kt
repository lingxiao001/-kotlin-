import java.io.File

// AST节点基类
abstract class ASTNode {
    abstract fun accept(visitor: ASTVisitor): String
}

// 访问者模式接口
interface ASTVisitor {
    fun visitProgram(node: Program): String
    fun visitFunction(node: FunctionDeclaration): String
    fun visitVariable(node: VariableDeclaration): String
    fun visitStatement(node: Statement): String
    fun visitExpression(node: Expression): String
    fun visitBlock(node: BlockStatement): String
    fun visitIf(node: IfStatement): String
    fun visitWhile(node: WhileStatement): String
    fun visitFor(node: ForStatement): String
    fun visitReturn(node: ReturnStatement): String
    fun visitBinary(node: BinaryExpression): String
    fun visitUnary(node: UnaryExpression): String
    fun visitCall(node: CallExpression): String
    fun visitIdentifier(node: Identifier): String
    fun visitLiteral(node: LiteralExpression): String
    fun visitAssignment(node: AssignmentExpression): String
}

// 程序根节点
data class Program(val declarations: List<Declaration>) : ASTNode() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitProgram(this)
}

// 声明基类
abstract class Declaration : ASTNode()

// 函数声明
data class FunctionDeclaration(
    val returnType: String,
    val name: String,
    val parameters: List<Parameter>,
    val body: BlockStatement?
) : Declaration() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitFunction(this)
}

// 参数
data class Parameter(val type: String, val name: String)

// 变量声明
data class VariableDeclaration(
    val type: String,
    val name: String,
    val initializer: Expression?
) : Declaration() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitVariable(this)
}

// 语句基类
abstract class Statement : ASTNode() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitStatement(this)
}

// 块语句
data class BlockStatement(val statements: List<Statement>) : Statement() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitBlock(this)
}

// 表达式语句
data class ExpressionStatement(val expression: Expression) : Statement()

// if语句
data class IfStatement(
    val condition: Expression,
    val thenStatement: Statement,
    val elseStatement: Statement?
) : Statement() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitIf(this)
}

// while语句
data class WhileStatement(
    val condition: Expression,
    val body: Statement
) : Statement() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitWhile(this)
}

// for语句
data class ForStatement(
    val init: Statement?,
    val condition: Expression?,
    val update: Expression?,
    val body: Statement
) : Statement() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitFor(this)
}

// return语句
data class ReturnStatement(val expression: Expression?) : Statement() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitReturn(this)
}

// 表达式基类
abstract class Expression : ASTNode() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitExpression(this)
}

// 二元表达式
data class BinaryExpression(
    val left: Expression,
    val operator: Token,
    val right: Expression
) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitBinary(this)
}

// 一元表达式
data class UnaryExpression(
    val operator: Token,
    val operand: Expression
) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitUnary(this)
}

// 赋值表达式
data class AssignmentExpression(
    val target: Expression,
    val operator: Token,
    val value: Expression
) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitAssignment(this)
}

// 函数调用表达式
data class CallExpression(
    val callee: Expression,
    val arguments: List<Expression>
) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitCall(this)
}

// 标识符表达式
data class Identifier(val name: String) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitIdentifier(this)
}

// 字面量表达式
data class LiteralExpression(val value: Any?, val type: TokenType) : Expression() {
    override fun accept(visitor: ASTVisitor): String = visitor.visitLiteral(this)
}

// 语法分析异常
class ParseException(message: String, val token: Token) : Exception("Parse error at ${token.position}: $message")

// 语法分析器
class CParser(private val tokens: List<Token>) {
    private var current = 0

    fun parse(): Program {
        val declarations = mutableListOf<Declaration>()

        while (!isAtEnd()) {
            // 跳过注释和换行
            if (check(TokenType.COMMENT) || check(TokenType.NEWLINE)) {
                advance()
                continue
            }

            try {
                declarations.add(declaration())
            } catch (e: ParseException) {
                println("Parse error: ${e.message}")
                synchronize()
            }
        }

        return Program(declarations)
    }

    private fun declaration(): Declaration {
        return when {
            checkType() -> {
                val type = advance().lexeme
                val name = consume(TokenType.IDENTIFIER, "Expected identifier").lexeme

                if (check(TokenType.LEFT_PAREN)) {
                    functionDeclaration(type, name)
                } else {
                    variableDeclaration(type, name)
                }
            }
            else -> throw ParseException("Expected declaration", peek())
        }
    }

    private fun functionDeclaration(returnType: String, name: String): FunctionDeclaration {
        consume(TokenType.LEFT_PAREN, "Expected '(' after function name")

        val parameters = mutableListOf<Parameter>()
        if (!check(TokenType.RIGHT_PAREN)) {
            do {
                val paramType = consume(TokenType.IDENTIFIER, "Expected parameter type").lexeme
                val paramName = consume(TokenType.IDENTIFIER, "Expected parameter name").lexeme
                parameters.add(Parameter(paramType, paramName))
            } while (match(TokenType.COMMA))
        }

        consume(TokenType.RIGHT_PAREN, "Expected ')' after parameters")

        val body = if (check(TokenType.LEFT_BRACE)) {
            blockStatement()
        } else {
            consume(TokenType.SEMICOLON, "Expected ';' after function declaration")
            null
        }

        return FunctionDeclaration(returnType, name, parameters, body)
    }

    private fun variableDeclaration(type: String, name: String): VariableDeclaration {
        val initializer = if (match(TokenType.ASSIGN)) {
            expression()
        } else null

        consume(TokenType.SEMICOLON, "Expected ';' after variable declaration")
        return VariableDeclaration(type, name, initializer)
    }

    private fun statement(): Statement {
        return when {
            match(TokenType.LEFT_BRACE) -> {
                current-- // 回退
                blockStatement()
            }
            match(TokenType.IF) -> ifStatement()
            match(TokenType.WHILE) -> whileStatement()
            match(TokenType.FOR) -> forStatement()
            match(TokenType.RETURN) -> returnStatement()
            else -> expressionStatement()
        }
    }

    private fun blockStatement(): BlockStatement {
        consume(TokenType.LEFT_BRACE, "Expected '{'")

        val statements = mutableListOf<Statement>()
        while (!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            // 跳过注释和换行
            if (check(TokenType.COMMENT) || check(TokenType.NEWLINE)) {
                advance()
                continue
            }

            if (checkType()) {
                // 局部变量声明
                val type = advance().lexeme
                val name = consume(TokenType.IDENTIFIER, "Expected identifier").lexeme
                val initializer = if (match(TokenType.ASSIGN)) expression() else null
                consume(TokenType.SEMICOLON, "Expected ';'")
                statements.add(ExpressionStatement(LiteralExpression("var_decl: $type $name", TokenType.IDENTIFIER)))
            } else {
                statements.add(statement())
            }
        }

        consume(TokenType.RIGHT_BRACE, "Expected '}'")
        return BlockStatement(statements)
    }

    private fun ifStatement(): IfStatement {
        consume(TokenType.LEFT_PAREN, "Expected '(' after 'if'")
        val condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expected ')' after if condition")

        val thenStatement = statement()
        val elseStatement = if (match(TokenType.ELSE)) statement() else null

        return IfStatement(condition, thenStatement, elseStatement)
    }

    private fun whileStatement(): WhileStatement {
        consume(TokenType.LEFT_PAREN, "Expected '(' after 'while'")
        val condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expected ')' after while condition")

        val body = statement()
        return WhileStatement(condition, body)
    }

    private fun forStatement(): ForStatement {
        consume(TokenType.LEFT_PAREN, "Expected '(' after 'for'")

        val init = if (match(TokenType.SEMICOLON)) null else {
            val stmt = expressionStatement()
            stmt
        }

        val condition = if (check(TokenType.SEMICOLON)) null else expression()
        consume(TokenType.SEMICOLON, "Expected ';' after for condition")

        val update = if (check(TokenType.RIGHT_PAREN)) null else expression()
        consume(TokenType.RIGHT_PAREN, "Expected ')' after for clauses")

        val body = statement()
        return ForStatement(init, condition, update, body)
    }

    private fun returnStatement(): ReturnStatement {
        val expr = if (check(TokenType.SEMICOLON)) null else expression()
        consume(TokenType.SEMICOLON, "Expected ';' after return value")
        return ReturnStatement(expr)
    }

    private fun expressionStatement(): ExpressionStatement {
        val expr = expression()
        consume(TokenType.SEMICOLON, "Expected ';' after expression")
        return ExpressionStatement(expr)
    }

    private fun expression(): Expression {
        return assignment()
    }

    private fun assignment(): Expression {
        val expr = logicalOr()

        if (matchAssignmentOperator()) {
            val operator = previous()
            val value = assignment()
            return AssignmentExpression(expr, operator, value)
        }

        return expr
    }

    private fun logicalOr(): Expression {
        var expr = logicalAnd()

        while (match(TokenType.LOGICAL_OR)) {
            val operator = previous()
            val right = logicalAnd()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun logicalAnd(): Expression {
        var expr = equality()

        while (match(TokenType.LOGICAL_AND)) {
            val operator = previous()
            val right = equality()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun equality(): Expression {
        var expr = comparison()

        while (match(TokenType.NOT_EQUAL, TokenType.EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun comparison(): Expression {
        var expr = term()

        while (match(TokenType.GREATER_THAN, TokenType.GREATER_EQUAL, TokenType.LESS_THAN, TokenType.LESS_EQUAL)) {
            val operator = previous()
            val right = term()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun term(): Expression {
        var expr = factor()

        while (match(TokenType.MINUS, TokenType.PLUS)) {
            val operator = previous()
            val right = factor()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun factor(): Expression {
        var expr = unary()

        while (match(TokenType.DIVIDE, TokenType.MULTIPLY)) {
            val operator = previous()
            val right = unary()
            expr = BinaryExpression(expr, operator, right)
        }

        return expr
    }

    private fun unary(): Expression {
        if (match(TokenType.LOGICAL_NOT, TokenType.MINUS, TokenType.PLUS)) {
            val operator = previous()
            val right = unary()
            return UnaryExpression(operator, right)
        }

        return call()
    }

    private fun call(): Expression {
        var expr = primary()

        while (true) {
            when {
                match(TokenType.LEFT_PAREN) -> {
                    expr = finishCall(expr)
                }
                else -> break
            }
        }

        return expr
    }

    private fun finishCall(callee: Expression): Expression {
        val arguments = mutableListOf<Expression>()

        if (!check(TokenType.RIGHT_PAREN)) {
            do {
                arguments.add(expression())
            } while (match(TokenType.COMMA))
        }

        consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments")
        return CallExpression(callee, arguments)
    }

    private fun primary(): Expression {
        when {
            match(TokenType.INTEGER_LITERAL) -> {
                return LiteralExpression(previous().value, TokenType.INTEGER_LITERAL)
            }
            match(TokenType.FLOAT_LITERAL) -> {
                return LiteralExpression(previous().value, TokenType.FLOAT_LITERAL)
            }
            match(TokenType.STRING_LITERAL) -> {
                return LiteralExpression(previous().value, TokenType.STRING_LITERAL)
            }
            match(TokenType.CHAR_LITERAL) -> {
                return LiteralExpression(previous().value, TokenType.CHAR_LITERAL)
            }
            match(TokenType.IDENTIFIER) -> {
                return Identifier(previous().lexeme)
            }
            match(TokenType.LEFT_PAREN) -> {
                val expr = expression()
                consume(TokenType.RIGHT_PAREN, "Expected ')' after expression")
                return expr
            }
        }

        throw ParseException("Expected expression", peek())
    }

    // 辅助方法
    private fun match(vararg types: TokenType): Boolean {
        for (type in types) {
            if (check(type)) {
                advance()
                return true
            }
        }
        return false
    }

    private fun matchAssignmentOperator(): Boolean {
        return match(TokenType.ASSIGN, TokenType.PLUS_ASSIGN, TokenType.MINUS_ASSIGN,
            TokenType.MULTIPLY_ASSIGN, TokenType.DIVIDE_ASSIGN, TokenType.MODULO_ASSIGN)
    }

    private fun check(type: TokenType): Boolean {
        if (isAtEnd()) return false
        return peek().type == type
    }

    private fun checkType(): Boolean {
        return check(TokenType.INT) || check(TokenType.FLOAT) || check(TokenType.DOUBLE) ||
                check(TokenType.CHAR) || check(TokenType.VOID) || check(TokenType.LONG) ||
                check(TokenType.SHORT) || check(TokenType.UNSIGNED) || check(TokenType.SIGNED)
    }

    private fun advance(): Token {
        if (!isAtEnd()) current++
        return previous()
    }

    private fun isAtEnd(): Boolean {
        return peek().type == TokenType.EOF
    }

    private fun peek(): Token {
        return tokens[current]
    }

    private fun previous(): Token {
        return tokens[current - 1]
    }

    private fun consume(type: TokenType, message: String): Token {
        if (check(type)) return advance()
        throw ParseException(message, peek())
    }

    private fun synchronize() {
        advance()

        while (!isAtEnd()) {
            if (previous().type == TokenType.SEMICOLON) return

            when (peek().type) {
                TokenType.IF, TokenType.FOR, TokenType.WHILE, TokenType.RETURN -> return
                else -> {}
            }

            advance()
        }
    }
}

// AST打印访问者
class ASTPrintVisitor(private val indent: String = "") : ASTVisitor {
    private var currentIndent = ""

    override fun visitProgram(node: Program): String {
        val sb = StringBuilder()
        sb.appendLine("Program")
        currentIndent = "  "
        for (decl in node.declarations) {
            sb.appendLine("${currentIndent}${decl.accept(this)}")
        }
        return sb.toString()
    }

    override fun visitFunction(node: FunctionDeclaration): String {
        val sb = StringBuilder()
        sb.append("FunctionDeclaration(${node.returnType} ${node.name})")
        if (node.parameters.isNotEmpty()) {
            sb.append(" params: ${node.parameters.joinToString { "${it.type} ${it.name}" }}")
        }
        if (node.body != null) {
            val oldIndent = currentIndent
            currentIndent += "  "
            sb.appendLine()
            sb.append("${currentIndent}${node.body.accept(this)}")
            currentIndent = oldIndent
        }
        return sb.toString()
    }

    override fun visitVariable(node: VariableDeclaration): String {
        val init = if (node.initializer != null) " = ${node.initializer.accept(this)}" else ""
        return "VariableDeclaration(${node.type} ${node.name}$init)"
    }

    override fun visitStatement(node: Statement): String {
        return when (node) {
            is BlockStatement -> node.accept(this)
            is ExpressionStatement -> "ExpressionStatement(${node.expression.accept(this)})"
            is IfStatement -> node.accept(this)
            is WhileStatement -> node.accept(this)
            is ForStatement -> node.accept(this)
            is ReturnStatement -> node.accept(this)
            else -> "Statement"
        }
    }

    override fun visitBlock(node: BlockStatement): String {
        val sb = StringBuilder()
        sb.append("Block")
        val oldIndent = currentIndent
        currentIndent += "  "
        for (stmt in node.statements) {
            sb.appendLine()
            sb.append("${currentIndent}${stmt.accept(this)}")
        }
        currentIndent = oldIndent
        return sb.toString()
    }

    override fun visitIf(node: IfStatement): String {
        val sb = StringBuilder()
        sb.append("IfStatement")
        val oldIndent = currentIndent
        currentIndent += "  "
        sb.appendLine()
        sb.append("${currentIndent}condition: ${node.condition.accept(this)}")
        sb.appendLine()
        sb.append("${currentIndent}then: ${node.thenStatement.accept(this)}")
        if (node.elseStatement != null) {
            sb.appendLine()
            sb.append("${currentIndent}else: ${node.elseStatement.accept(this)}")
        }
        currentIndent = oldIndent
        return sb.toString()
    }

    override fun visitWhile(node: WhileStatement): String {
        val sb = StringBuilder()
        sb.append("WhileStatement")
        val oldIndent = currentIndent
        currentIndent += "  "
        sb.appendLine()
        sb.append("${currentIndent}condition: ${node.condition.accept(this)}")
        sb.appendLine()
        sb.append("${currentIndent}body: ${node.body.accept(this)}")
        currentIndent = oldIndent
        return sb.toString()
    }

    override fun visitFor(node: ForStatement): String {
        val sb = StringBuilder()
        sb.append("ForStatement")
        val oldIndent = currentIndent
        currentIndent += "  "
        if (node.init != null) {
            sb.appendLine()
            sb.append("${currentIndent}init: ${node.init.accept(this)}")
        }
        if (node.condition != null) {
            sb.appendLine()
            sb.append("${currentIndent}condition: ${node.condition.accept(this)}")
        }
        if (node.update != null) {
            sb.appendLine()
            sb.append("${currentIndent}update: ${node.update.accept(this)}")
        }
        sb.appendLine()
        sb.append("${currentIndent}body: ${node.body.accept(this)}")
        currentIndent = oldIndent
        return sb.toString()
    }

    override fun visitReturn(node: ReturnStatement): String {
        val expr = if (node.expression != null) node.expression.accept(this) else "void"
        return "ReturnStatement($expr)"
    }

    override fun visitExpression(node: Expression): String {
        return when (node) {
            is BinaryExpression -> node.accept(this)
            is UnaryExpression -> node.accept(this)
            is AssignmentExpression -> node.accept(this)
            is CallExpression -> node.accept(this)
            is Identifier -> node.accept(this)
            is LiteralExpression -> node.accept(this)
            else -> "Expression"
        }
    }

    override fun visitBinary(node: BinaryExpression): String {
        return "BinaryExpr(${node.left.accept(this)} ${node.operator.lexeme} ${node.right.accept(this)})"
    }

    override fun visitUnary(node: UnaryExpression): String {
        return "UnaryExpr(${node.operator.lexeme} ${node.operand.accept(this)})"
    }

    override fun visitAssignment(node: AssignmentExpression): String {
        return "Assignment(${node.target.accept(this)} ${node.operator.lexeme} ${node.value.accept(this)})"
    }

    override fun visitCall(node: CallExpression): String {
        val args = node.arguments.joinToString(", ") { it.accept(this) }
        return "Call(${node.callee.accept(this)}($args))"
    }

    override fun visitIdentifier(node: Identifier): String {
        return "ID(${node.name})"
    }

    override fun visitLiteral(node: LiteralExpression): String {
        return "Literal(${node.value})"
    }
}

// 生成中间代码访问者
class IntermediateCodeVisitor : ASTVisitor {
    private val code = mutableListOf<String>()
    private var tempCounter = 0
    private var labelCounter = 0

    fun getCode(): List<String> = code

    private fun newTemp(): String = "t${tempCounter++}"
    private fun newLabel(): String = "L${labelCounter++}"

    override fun visitProgram(node: Program): String {
        for (decl in node.declarations) {
            decl.accept(this)
        }
        return ""
    }

    override fun visitFunction(node: FunctionDeclaration): String {
        code.add("FUNC ${node.name}:")
        if (node.body != null) {
            node.body.accept(this)
        }
        code.add("ENDFUNC")
        return ""
    }

    override fun visitVariable(node: VariableDeclaration): String {
        if (node.initializer != null) {
            val temp = node.initializer.accept(this)
            code.add("STORE ${node.name}, $temp")
        } else {
            code.add("DECLARE ${node.type} ${node.name}")
        }
        return ""
    }

    override fun visitStatement(node: Statement): String {
        return when (node) {
            is ExpressionStatement -> {
                node.expression.accept(this)
                ""
            }
            else -> node.accept(this)
        }
    }

    override fun visitBlock(node: BlockStatement): String {
        for (stmt in node.statements) {
            stmt.accept(this)
        }
        return ""
    }

    override fun visitIf(node: IfStatement): String {
        val condTemp = node.condition.accept(this)
        val elseLabel = newLabel()
        val endLabel = newLabel()

        code.add("IF_FALSE $condTemp GOTO $elseLabel")
        node.thenStatement.accept(this)
        code.add("GOTO $endLabel")
        code.add("$elseLabel:")
        node.elseStatement?.accept(this)
        code.add("$endLabel:")

        return ""
    }

    override fun visitWhile(node: WhileStatement): String {
        val startLabel = newLabel()
        val endLabel = newLabel()

        code.add("$startLabel:")
        val condTemp = node.condition.accept(this)
        code.add("IF_FALSE $condTemp GOTO $endLabel")
        node.body.accept(this)
        code.add("GOTO $startLabel")
        code.add("$endLabel:")

        return ""
    }

    override fun visitFor(node: ForStatement): String {
        node.init?.accept(this)
        val startLabel = newLabel()
        val endLabel = newLabel()

        code.add("$startLabel:")
        if (node.condition != null) {
            val condTemp = node.condition.accept(this)
            code.add("IF_FALSE $condTemp GOTO $endLabel")
        }
        node.body.accept(this)
        node.update?.accept(this)
        code.add("GOTO $startLabel")
        code.add("$endLabel:")

        return ""
    }

    override fun visitReturn(node: ReturnStatement): String {
        if (node.expression != null) {
            val temp = node.expression.accept(this)
            code.add("RETURN $temp")
        } else {
            code.add("RETURN")
        }
        return ""
    }

    override fun visitExpression(node: Expression): String {
        return node.accept(this)
    }

    override fun visitBinary(node: BinaryExpression): String {
        val leftTemp = node.left.accept(this)
        val rightTemp = node.right.accept(this)
        val resultTemp = newTemp()

        val op = when (node.operator.type) {
            TokenType.PLUS -> "ADD"
            TokenType.MINUS -> "SUB"
            TokenType.MULTIPLY -> "MUL"
            TokenType.DIVIDE -> "DIV"
            TokenType.MODULO -> "MOD"
            TokenType.EQUAL -> "EQ"
            TokenType.NOT_EQUAL -> "NE"
            TokenType.LESS_THAN -> "LT"
            TokenType.GREATER_THAN -> "GT"
            TokenType.LESS_EQUAL -> "LE"
            TokenType.GREATER_EQUAL -> "GE"
            TokenType.LOGICAL_AND -> "AND"
            TokenType.LOGICAL_OR -> "OR"
            else -> "OP"
        }

        code.add("$resultTemp = $op $leftTemp, $rightTemp")
        return resultTemp
    }

    override fun visitUnary(node: UnaryExpression): String {
        val operandTemp = node.operand.accept(this)
        val resultTemp = newTemp()

        val op = when (node.operator.type) {
            TokenType.MINUS -> "NEG"
            TokenType.LOGICAL_NOT -> "NOT"
            else -> "UNARY"
        }

        code.add("$resultTemp = $op $operandTemp")
        return resultTemp
    }

    override fun visitAssignment(node: AssignmentExpression): String {
        val valueTemp = node.value.accept(this)
        val targetName = when (node.target) {
            is Identifier -> node.target.name
            else -> "temp"
        }

        code.add("STORE $targetName, $valueTemp")
        return valueTemp
    }

    override fun visitCall(node: CallExpression): String {
        val argTemps = mutableListOf<String>()
        for (arg in node.arguments) {
            argTemps.add(arg.accept(this))
        }

        val funcName = when (node.callee) {
            is Identifier -> node.callee.name
            else -> "unknown"
        }

        for (argTemp in argTemps) {
            code.add("PARAM $argTemp")
        }

        val resultTemp = newTemp()
        code.add("$resultTemp = CALL $funcName, ${argTemps.size}")

        return resultTemp
    }

    override fun visitIdentifier(node: Identifier): String {
        return node.name
    }

    override fun visitLiteral(node: LiteralExpression): String {
        return node.value.toString()
    }
}

// 主程序扩展
fun parseAndAnalyze() {
    val tokenFile = File("lexertest_tokens.txt")

    if (!tokenFile.exists()) {
        println("Error: Token file not found. Please run lexer first.")
        return
    }

    try {
        // 读取token文件并重构tokens
        val tokens = mutableListOf<Token>()
        tokenFile.readLines().forEach { line ->
            if (line.startsWith("#") || line.isBlank()) return@forEach

            val parts = line.split("|")
            if (parts.size >= 2) {
                val tokenType = TokenType.valueOf(parts[0])
                val lexeme = parts[1]
                val position = Position(1, 1, 0) // 简化位置信息
                tokens.add(Token(tokenType, lexeme, position))
            }
        }

        // 语法分析
        val parser = CParser(tokens)
        val ast = parser.parse()

        // 打印语法树
        println("=== Abstract Syntax Tree ===")
        val printVisitor = ASTPrintVisitor()
        println(ast.accept(printVisitor))

        // 生成中间代码
        println("\n=== Intermediate Code ===")
        val icVisitor = IntermediateCodeVisitor()
        ast.accept(icVisitor)
        val intermediateCode = icVisitor.getCode()

        for (instruction in intermediateCode) {
            println(instruction)
        }

        // 生成AST文件
        val astFile = File("lexertest_ast.txt")
        astFile.writeText(generateASTFile(ast))
        println("\nAST file generated: ${astFile.name}")

        // 生成中间代码文件
        val icFile = File("lexertest_intermediate.txt")
        icFile.writeText(generateIntermediateFile(intermediateCode))
        println("Intermediate code file generated: ${icFile.name}")

        // 生成符号表文件（用于语义分析）
        val symbolTableFile = File("lexertest_symboltable.txt")
        symbolTableFile.writeText(generateSymbolTableFile(ast))
        println("Symbol table file generated: ${symbolTableFile.name}")

    } catch (e: Exception) {
        println("Parser Error: ${e.message}")
        e.printStackTrace()
    }
}

// 生成AST文件
fun generateASTFile(ast: Program): String {
    val sb = StringBuilder()
    sb.appendLine("# Abstract Syntax Tree")
    sb.appendLine("# Generated by C Parser")
    sb.appendLine("# Format: Node structure with indentation")
    sb.appendLine()

    val visitor = ASTPrintVisitor()
    sb.append(ast.accept(visitor))

    return sb.toString()
}

// 生成中间代码文件
fun generateIntermediateFile(code: List<String>): String {
    val sb = StringBuilder()
    sb.appendLine("# Intermediate Code")
    sb.appendLine("# Three-Address Code Format")
    sb.appendLine("# Generated for Semantic Analysis")
    sb.appendLine()

//    code.forEachIndexed { index, instruction ->
//        sb.appendLine("${index + 1:3}: $instruction")
//    }
    code.forEachIndexed { index, instruction ->
        sb.appendLine("${String.format("%3d", index + 1)}: $instruction")
    }

    return sb.toString()
}

// 生成符号表文件（用于语义分析）
fun generateSymbolTableFile(ast: Program): String {
    val sb = StringBuilder()
    sb.appendLine("# Symbol Table for Semantic Analysis")
    sb.appendLine("# FORMAT: SYMBOL_NAME|SYMBOL_TYPE|SCOPE|DECLARATION_LINE")
    sb.appendLine()

    val symbolCollector = SymbolCollector()
    ast.accept(symbolCollector)

    val symbols = symbolCollector.getSymbols()
    for ((name, info) in symbols) {
        sb.appendLine("$name|${info.type}|${info.scope}|${info.line}")
    }

    return sb.toString()
}

// 符号信息类
data class SymbolInfo(
    val type: String,
    val scope: String,
    val line: Int,
    val kind: String // "FUNCTION", "VARIABLE", "PARAMETER"
)

// 符号收集访问者
class SymbolCollector : ASTVisitor {
    private val symbols = mutableMapOf<String, SymbolInfo>()
    private var currentScope = "global"
    private var currentLine = 1

    fun getSymbols(): Map<String, SymbolInfo> = symbols

    override fun visitProgram(node: Program): String {
        for (decl in node.declarations) {
            decl.accept(this)
        }
        return ""
    }

    override fun visitFunction(node: FunctionDeclaration): String {
        // 添加函数符号
        symbols[node.name] = SymbolInfo(
            type = node.returnType,
            scope = currentScope,
            line = currentLine,
            kind = "FUNCTION"
        )

        val oldScope = currentScope
        currentScope = node.name

        // 添加参数符号
        for (param in node.parameters) {
            symbols[param.name] = SymbolInfo(
                type = param.type,
                scope = currentScope,
                line = currentLine,
                kind = "PARAMETER"
            )
        }

        // 处理函数体
        node.body?.accept(this)

        currentScope = oldScope
        return ""
    }

    override fun visitVariable(node: VariableDeclaration): String {
        symbols[node.name] = SymbolInfo(
            type = node.type,
            scope = currentScope,
            line = currentLine,
            kind = "VARIABLE"
        )

        node.initializer?.accept(this)
        return ""
    }

    override fun visitStatement(node: Statement): String {
        return when (node) {
            is BlockStatement -> node.accept(this)
            is ExpressionStatement -> node.expression.accept(this)
            is IfStatement -> node.accept(this)
            is WhileStatement -> node.accept(this)
            is ForStatement -> node.accept(this)
            is ReturnStatement -> node.accept(this)
            else -> ""
        }
    }

    override fun visitBlock(node: BlockStatement): String {
        for (stmt in node.statements) {
            stmt.accept(this)
        }
        return ""
    }

    override fun visitIf(node: IfStatement): String {
        node.condition.accept(this)
        node.thenStatement.accept(this)
        node.elseStatement?.accept(this)
        return ""
    }

    override fun visitWhile(node: WhileStatement): String {
        node.condition.accept(this)
        node.body.accept(this)
        return ""
    }

    override fun visitFor(node: ForStatement): String {
        node.init?.accept(this)
        node.condition?.accept(this)
        node.update?.accept(this)
        node.body.accept(this)
        return ""
    }

    override fun visitReturn(node: ReturnStatement): String {
        node.expression?.accept(this)
        return ""
    }

    override fun visitExpression(node: Expression): String {
        return node.accept(this)
    }

    override fun visitBinary(node: BinaryExpression): String {
        node.left.accept(this)
        node.right.accept(this)
        return ""
    }

    override fun visitUnary(node: UnaryExpression): String {
        node.operand.accept(this)
        return ""
    }

    override fun visitAssignment(node: AssignmentExpression): String {
        node.target.accept(this)
        node.value.accept(this)
        return ""
    }

    override fun visitCall(node: CallExpression): String {
        node.callee.accept(this)
        for (arg in node.arguments) {
            arg.accept(this)
        }
        return ""
    }

    override fun visitIdentifier(node: Identifier): String {
        // 标识符使用，不需要添加到符号表
        return ""
    }

    override fun visitLiteral(node: LiteralExpression): String {
        return ""
    }
}

// 扩展主程序，集成词法分析和语法分析
fun main() {
    val sourceFile = File("C:\\Users\\18319\\IdeaProjects\\kotlinCompiler\\src\\lackof}.c")

    if (!sourceFile.exists()) {
        println("Error: File lexertest.c not found")
        return
    }

    try {
        println("=== C Compiler - Lexical and Syntax Analysis ===")

        // 步骤1: 词法分析
        println("\n1. Lexical Analysis...")
        val sourceCode = sourceFile.readText()
        val lexer = CLexer(sourceCode)
        val tokens = lexer.tokenize()

        // 输出词法分析结果
        println("Tokens generated: ${tokens.size}")

        // 生成token文件
        val tokenFile = File("lexertest_tokens.txt")
        tokenFile.writeText(generateTokenFileForParser(tokens))

        // 步骤2: 语法分析
        println("\n2. Syntax Analysis...")
        val parser = CParser(tokens.filter { it.type != TokenType.COMMENT && it.type != TokenType.NEWLINE })
        val ast = parser.parse()

        // 打印语法树
        println("\n=== Abstract Syntax Tree ===")
        val printVisitor = ASTPrintVisitor()
        val astString = ast.accept(printVisitor)
        println(astString)

        // 生成中间代码
        println("\n=== Intermediate Code ===")
        val icVisitor = IntermediateCodeVisitor()
        ast.accept(icVisitor)
        val intermediateCode = icVisitor.getCode()

        intermediateCode.forEachIndexed { index, instruction ->
            println("${String.format("%3d", index + 1)}: $instruction")
        }

        // 生成输出文件
        println("\n3. Generating output files...")

        // AST文件
        val astFile = File("lexertest_ast.txt")
        astFile.writeText(generateASTFile(ast))
        println("✓ AST file: ${astFile.name}")

        // 中间代码文件
        val icFile = File("lexertest_intermediate.txt")
        icFile.writeText(generateIntermediateFile(intermediateCode))
        println("✓ Intermediate code file: ${icFile.name}")

        // 符号表文件
        val symbolTableFile = File("lexertest_symboltable.txt")
        symbolTableFile.writeText(generateSymbolTableFile(ast))
        println("✓ Symbol table file: ${symbolTableFile.name}")

        // 统计信息
        println("\n=== Compilation Statistics ===")
        println("Source lines: ${sourceCode.lines().size}")
        println("Tokens: ${tokens.size}")
        println("AST nodes: ${countASTNodes(ast)}")
        println("Intermediate instructions: ${intermediateCode.size}")

        val symbolCollector = SymbolCollector()
        ast.accept(symbolCollector)
        println("Symbols: ${symbolCollector.getSymbols().size}")

    } catch (e: LexerException) {
        println("Lexical Error: ${e.message}")
    } catch (e: ParseException) {
        println("Parse Error: ${e.message}")
    } catch (e: Exception) {
        println("Compilation Error: ${e.message}")
        e.printStackTrace()
    }
}

// 为语法分析器生成token文件
fun generateTokenFileForParser(tokens: List<Token>): String {
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

// 计算AST节点数量
fun countASTNodes(node: ASTNode): Int {
    return when (node) {
        is Program -> 1 + node.declarations.sumOf { countASTNodes(it) }
        is FunctionDeclaration -> 1 + (node.body?.let { countASTNodes(it) } ?: 0)
        is VariableDeclaration -> 1 + (node.initializer?.let { countASTNodes(it) } ?: 0)
        is BlockStatement -> 1 + node.statements.sumOf { countASTNodes(it) }
        is IfStatement -> 1 + countASTNodes(node.condition) + countASTNodes(node.thenStatement) +
                (node.elseStatement?.let { countASTNodes(it) } ?: 0)
        is WhileStatement -> 1 + countASTNodes(node.condition) + countASTNodes(node.body)
        is ForStatement -> 1 + (node.init?.let { countASTNodes(it) } ?: 0) +
                (node.condition?.let { countASTNodes(it) } ?: 0) +
                (node.update?.let { countASTNodes(it) } ?: 0) + countASTNodes(node.body)
        is ReturnStatement -> 1 + (node.expression?.let { countASTNodes(it) } ?: 0)
        is ExpressionStatement -> 1 + countASTNodes(node.expression)
        is BinaryExpression -> 1 + countASTNodes(node.left) + countASTNodes(node.right)
        is UnaryExpression -> 1 + countASTNodes(node.operand)
        is AssignmentExpression -> 1 + countASTNodes(node.target) + countASTNodes(node.value)
        is CallExpression -> 1 + countASTNodes(node.callee) + node.arguments.sumOf { countASTNodes(it) }
        else -> 1
    }
}