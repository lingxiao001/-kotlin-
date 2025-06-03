// 符号表项
import TokenType

//符号表的抽象接口
sealed class Symbol {
    abstract val name: String
    abstract val type: String
}

data class VariableSymbol(
    override val name: String,
    override val type: String,
    var initialized: Boolean = false
) : Symbol()

data class FunctionSymbol(
    override val name: String,
    override val type: String,
    val parameters: List<Parameter>
) : Symbol()

// 作用域
class Scope(val parent: Scope? = null) {
    private val symbols = mutableMapOf<String, Symbol>()

    fun define(symbol: Symbol) {
        symbols[symbol.name] = symbol
    }

    fun resolve(name: String): Symbol? {
        return symbols[name] ?: parent?.resolve(name)
    }

    fun resolveCurrentScopeOnly(name: String): Symbol? {
        return symbols[name]
    }
}

// 语义错误
class SemanticError(message: String, val node: ASTNode) : Exception(message)

// 语义分析器
class SemanticAnalyzer : ASTVisitor {
    private var currentScope: Scope = Scope()
    private var currentFunction: FunctionSymbol? = null
    private val errors = mutableListOf<SemanticError>()
    private var hasMainFunction = false  // 添加对main函数的检查

    // 添加预定义的标准库函数--直接调用无需声明
    fun defineStandardLibrary() {
        // printf 函数
        currentScope.define(FunctionSymbol(
            name = "printf",
            type = "int",
            parameters = listOf(Parameter("string", "format"))
        ))
        
        // scanf 函数
        currentScope.define(FunctionSymbol(
            name = "scanf",
            type = "int",
            parameters = listOf(Parameter("string", "format"))
        ))
        
        // malloc 函数
        currentScope.define(FunctionSymbol(
            name = "malloc",
            type = "void*",
            parameters = listOf(Parameter("int", "size"))
        ))
        
        // free 函数
        currentScope.define(FunctionSymbol(
            name = "free",
            type = "void",
            parameters = listOf(Parameter("void*", "ptr"))
        ))
    }

    // 基本类型检查
    private fun isCompatibleType(expected: String, actual: String): Boolean {
        if (expected == actual) return true
        // 数值类型之间的兼容性
        if ((expected == "int" || expected == "float") && (actual == "int" || actual == "float")) {
            return true
        }
        // 条件语句中的类型兼容性
        if (expected == "bool" && (actual == "int" || actual == "float")) {
            return true
        }
        return false
    }

    private fun addError(message: String, node: ASTNode) {
        errors.add(SemanticError(message, node))
    }

    // 分析入口
    fun analyze(program: Program) {
        defineStandardLibrary()
        
        try {
            program.accept(this)
            
            // 检查是否定义了main函数
            if (!hasMainFunction) {
                addError("Program must have a main function", program)
            }
        } catch (e: Exception) {
            // Handle any unexpected errors that aren't semantic errors
            addError("Unexpected error during analysis: ${e.message}", program)
        }
        
        if (errors.isNotEmpty()) {
            errors.forEach { println("Semantic Error: ${it.message}") }
            throw Exception("Semantic analysis failed with ${errors.size} errors")
        }
    }

    override fun visitProgram(node: Program): String {
        node.declarations.forEach { it.accept(this) }
        return ""
    }

    override fun visitFunction(node: FunctionDeclaration): String {
        // 检查函数重复定义 - 只在当前作用域检查
        if (currentScope.resolveCurrentScopeOnly(node.name) != null) {
            addError("Function ${node.name} is already defined in current scope", node)
            return node.returnType
        }

        // 检查是否是main函数
        if (node.name == "main") {
            hasMainFunction = true
            // 检查main函数的返回类型
            if (node.returnType != "int") {
                addError("Main function must return int", node)
            }
            // 检查main函数的参数
            if (node.parameters.isNotEmpty()) {
                addError("Main function should have no parameters", node)
            }
        }

        // 创建函数符号
        val functionSymbol = FunctionSymbol(node.name, node.returnType, node.parameters)
        currentScope.define(functionSymbol)

        // 创建新的作用域
        val functionScope = Scope(currentScope)
        val previousScope = currentScope
        currentScope = functionScope

        // 保存当前函数上下文
        val previousFunction = currentFunction
        currentFunction = functionSymbol

        // 添加参数到作用域
        node.parameters.forEach { param ->
            if (currentScope.resolveCurrentScopeOnly(param.name) != null) {
                addError("Parameter ${param.name} is already defined", node)
            } else {
                currentScope.define(VariableSymbol(param.name, param.type, true))
            }
        }

        // 分析函数体
        node.body?.accept(this)

        // 恢复作用域和函数上下文
        currentScope = previousScope
        currentFunction = previousFunction
        return node.returnType
    }

    override fun visitVariable(node: VariableDeclaration): String {
        // 检查变量重复定义 - 严格检查当前作用域
        val existingSymbol = currentScope.resolveCurrentScopeOnly(node.name)
        if (existingSymbol != null) {
            addError("Variable '${node.name}' is already defined in the current scope", node)
            return node.type
        }

        // 检查初始化表达式的类型
        if (node.initializer != null) {
            val initializerType = node.initializer.accept(this)
            if (!isCompatibleType(node.type, initializerType)) {
                addError(
                    "Type mismatch: cannot assign $initializerType to ${node.type}",
                    node
                )
            }
        }

        // 添加到符号表，标记初始化状态
        val variableSymbol = VariableSymbol(node.name, node.type, node.initializer != null)
        currentScope.define(variableSymbol)
        return node.type
    }

    override fun visitBlock(node: BlockStatement): String {
        val blockScope = Scope(currentScope)
        val previousScope = currentScope
        currentScope = blockScope

        node.statements.forEach { it.accept(this) }

        currentScope = previousScope
        return ""
    }

    override fun visitIf(node: IfStatement): String {
        // 检查条件表达式是否为布尔类型
        val conditionType = node.condition.accept(this)
        if (conditionType != "bool" && conditionType != "error") {
            addError("Condition must be boolean", node.condition)
        }

        node.thenStatement.accept(this)
        node.elseStatement?.accept(this)
        return ""
    }

    override fun visitWhile(node: WhileStatement): String {
        // 检查条件表达式是否为布尔类型
        val conditionType = node.condition.accept(this)
        if (conditionType != "bool") {
            throw SemanticError("Condition must be boolean", node.condition)
        }

        node.body.accept(this)
        return ""
    }

    override fun visitFor(node: ForStatement): String {
        val forScope = Scope(currentScope)
        val previousScope = currentScope
        currentScope = forScope

        node.init?.accept(this)
        
        if (node.condition != null) {
            val conditionType = node.condition.accept(this)
            if (conditionType != "bool") {
                throw SemanticError("For condition must be boolean", node.condition)
            }
        }

        node.update?.accept(this)
        node.body.accept(this)

        currentScope = previousScope
        return ""
    }

    override fun visitReturn(node: ReturnStatement): String {
        if (currentFunction == null) {
            throw SemanticError("Return statement outside function", node)
        }

        if (node.expression == null) {
            if (currentFunction!!.type != "void") {
                throw SemanticError(
                    "Function ${currentFunction!!.name} must return ${currentFunction!!.type}",
                    node
                )
            }
        } else {
            val returnType = node.expression.accept(this)
            if (!isCompatibleType(currentFunction!!.type, returnType)) {
                throw SemanticError(
                    "Cannot return $returnType from function of type ${currentFunction!!.type}",
                    node
                )
            }
        }
        return ""
    }

    override fun visitBinary(node: BinaryExpression): String {
        val leftType = node.left.accept(this)
        val rightType = node.right.accept(this)

        return when (node.operator.type) {
            TokenType.PLUS, TokenType.MINUS, TokenType.MULTIPLY, TokenType.DIVIDE -> {
                if (!isCompatibleType("int", leftType) || !isCompatibleType("int", rightType)) {
                    throw SemanticError("Arithmetic operators require numeric operands", node)
                }
                if (leftType == "float" || rightType == "float") "float" else "int"
            }
            TokenType.EQUAL, TokenType.NOT_EQUAL -> {
                if (!isCompatibleType(leftType, rightType)) {
                    throw SemanticError("Cannot compare incompatible types $leftType and $rightType", node)
                }
                "bool"
            }
            TokenType.LESS_THAN, TokenType.LESS_EQUAL, TokenType.GREATER_THAN, TokenType.GREATER_EQUAL -> {
                if (!isCompatibleType("int", leftType) || !isCompatibleType("int", rightType)) {
                    throw SemanticError("Comparison operators require numeric operands", node)
                }
                "bool"
            }
            TokenType.LOGICAL_AND, TokenType.LOGICAL_OR -> {
                if (!isCompatibleType("bool", leftType) || !isCompatibleType("bool", rightType)) {
                    throw SemanticError("Logical operators require boolean operands", node)
                }
                "bool"
            }
            else -> throw SemanticError("Unsupported binary operator ${node.operator.lexeme}", node)
        }
    }

    override fun visitUnary(node: UnaryExpression): String {
        val operandType = node.operand.accept(this)
        return when (node.operator.type) {
            TokenType.MINUS -> {
                if (operandType != "int" && operandType != "float") {
                    throw SemanticError("Unary minus requires numeric operand", node)
                }
                operandType
            }
            TokenType.LOGICAL_NOT -> {
                if (operandType != "bool") {
                    throw SemanticError("Logical not requires boolean operand", node)
                }
                "bool"
            }
            else -> throw SemanticError("Unsupported unary operator ${node.operator.lexeme}", node)
        }
    }

    override fun visitCall(node: CallExpression): String {
        // 检查调用的是否为函数
        val callee = node.callee
        if (callee !is Identifier) {
            throw SemanticError("Invalid function call", node)
        }

        val function = currentScope.resolve(callee.name)
        if (function !is FunctionSymbol) {
            throw SemanticError("${callee.name} is not a function", node)
        }

        // 检查参数数量
        if (node.arguments.size != function.parameters.size) {
            throw SemanticError(
                "Function ${function.name} expects ${function.parameters.size} arguments, but got ${node.arguments.size}",
                node
            )
        }

        // 检查参数类型
        node.arguments.zip(function.parameters).forEach { (arg, param) ->
            val argType = arg.accept(this)
            if (!isCompatibleType(param.type, argType)) {
                throw SemanticError(
                    "Cannot pass $argType as ${param.type} in call to ${function.name}",
                    arg
                )
            }
        }

        return function.type
    }

    override fun visitIdentifier(node: Identifier): String {
        val symbol = currentScope.resolve(node.name)
        if (symbol == null) {
            addError("Undefined variable '${node.name}'", node)
            return "error" // 返回一个错误类型，但继续分析
        }
        
        if (symbol is VariableSymbol && !symbol.initialized) {
            addError("Variable '${node.name}' might be used before initialization", node)
        }
        
        return symbol.type
    }

    override fun visitLiteral(node: LiteralExpression): String {
        return when (node.type) {
            TokenType.INTEGER_LITERAL -> "int"
            TokenType.FLOAT_LITERAL -> "float"
            TokenType.STRING_LITERAL -> "string"
            TokenType.TRUE, TokenType.FALSE -> "bool"
            TokenType.NULL -> "null"
            TokenType.IDENTIFIER -> "string"  // 处理标识符类型的字面量
            else -> {
                addError("Unsupported literal type: ${node.type}", node)
                "error"
            }
        }
    }

    override fun visitAssignment(node: AssignmentExpression): String {
        if (node.target !is Identifier) {
            addError("Invalid assignment target", node)
            return "error"
        }

        val targetName = (node.target as Identifier).name
        val symbol = currentScope.resolve(targetName)
        
        if (symbol == null) {
            addError("Undefined variable '$targetName'", node)
            return "error"
        }

        if (symbol !is VariableSymbol) {
            addError("Cannot assign to '$targetName'", node)
            return "error"
        }

        val valueType = node.value.accept(this)
        if (!isCompatibleType(symbol.type, valueType) && valueType != "error") {
            addError(
                "Type mismatch: cannot assign $valueType to variable of type ${symbol.type}",
                node
            )
        }

        // 标记变量已初始化
        symbol.initialized = true
        return valueType
    }

    override fun visitStatement(node: Statement): String {
        when (node) {
            is ExpressionStatement -> node.expression.accept(this)
            else -> node.accept(this)
        }
        return ""
    }

    override fun visitExpression(node: Expression): String {
        return node.accept(this)
    }
}
