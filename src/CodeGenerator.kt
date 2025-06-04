// 代码生成器 - 将AST转换为虚拟机指令
class CodeGenerator(private val vm: VirtualMachine) : ASTVisitor {
    private var labelCounter = 0
    private val functionLabels = mutableMapOf<String, Int>()

    // 生成新的标签
    private fun newLabel(): Int = labelCounter++

    // 访问程序根节点
    override fun visitProgram(node: Program): String {
        for (decl in node.declarations) {
            decl.accept(this)
        }
        return ""
    }

    // 访问函数声明
    override fun visitFunction(node: FunctionDeclaration): String {
        val functionLabel = newLabel()
        functionLabels[node.name] = functionLabel
        
        // 添加函数标签
        vm.addInstruction(Instruction(OpCode.PUSH, functionLabel))
        vm.addInstruction(Instruction(OpCode.JMP, functionLabel))
        
        // 处理函数体
        node.body?.accept(this)
        
        // 如果是void函数，确保有返回指令
        if (node.returnType == "void") {
            vm.addInstruction(Instruction(OpCode.PUSH, null))
            vm.addInstruction(Instruction(OpCode.RETURN))
        }
        
        return ""
    }

    // 访问变量声明
    override fun visitVariable(node: VariableDeclaration): String {
        if (node.initializer != null) {
            node.initializer.accept(this)
            vm.addInstruction(Instruction(OpCode.STORE, node.name))
        }
        return ""
    }

    // 访问语句
    override fun visitStatement(node: Statement): String {
        when (node) {
            is ExpressionStatement -> node.expression.accept(this)
            is BlockStatement -> node.accept(this)
            is IfStatement -> node.accept(this)
            is WhileStatement -> node.accept(this)
            is ForStatement -> node.accept(this)
            is ReturnStatement -> node.accept(this)
        }
        return ""
    }

    // 访问块语句
    override fun visitBlock(node: BlockStatement): String {
        for (stmt in node.statements) {
            stmt.accept(this)
        }
        return ""
    }

    // 访问if语句
    override fun visitIf(node: IfStatement): String {
        val elseLabel = newLabel()
        val endLabel = newLabel()

        // 生成条件判断代码
        node.condition.accept(this)
        vm.addInstruction(Instruction(OpCode.JZ, elseLabel))

        // 生成then分支代码
        node.thenStatement.accept(this)
        vm.addInstruction(Instruction(OpCode.JMP, endLabel))

        // 生成else分支代码
        vm.addInstruction(Instruction(OpCode.PUSH, elseLabel))
        if (node.elseStatement != null) {
            node.elseStatement.accept(this)
        }

        vm.addInstruction(Instruction(OpCode.PUSH, endLabel))
        return ""
    }

    // 访问while语句
    override fun visitWhile(node: WhileStatement): String {
        val startLabel = newLabel()
        val endLabel = newLabel()

        // 生成循环开始标签
        vm.addInstruction(Instruction(OpCode.PUSH, startLabel))

        // 生成条件判断代码
        node.condition.accept(this)
        vm.addInstruction(Instruction(OpCode.JZ, endLabel))

        // 生成循环体代码
        node.body.accept(this)
        vm.addInstruction(Instruction(OpCode.JMP, startLabel))

        // 生成循环结束标签
        vm.addInstruction(Instruction(OpCode.PUSH, endLabel))
        return ""
    }

    // 访问for语句
    override fun visitFor(node: ForStatement): String {
        val startLabel = newLabel()
        val endLabel = newLabel()

        // 生成初始化代码
        node.init?.accept(this)

        // 生成循环开始标签
        vm.addInstruction(Instruction(OpCode.PUSH, startLabel))

        // 生成条件判断代码
        if (node.condition != null) {
            node.condition.accept(this)
            vm.addInstruction(Instruction(OpCode.JZ, endLabel))
        }

        // 生成循环体代码
        node.body.accept(this)

        // 生成更新代码
        node.update?.accept(this)
        vm.addInstruction(Instruction(OpCode.JMP, startLabel))

        // 生成循环结束标签
        vm.addInstruction(Instruction(OpCode.PUSH, endLabel))
        return ""
    }

    // 访问return语句
    override fun visitReturn(node: ReturnStatement): String {
        if (node.expression != null) {
            node.expression.accept(this)
        } else {
            vm.addInstruction(Instruction(OpCode.PUSH, null))
        }
        vm.addInstruction(Instruction(OpCode.RETURN))
        return ""
    }

    // 访问表达式
    override fun visitExpression(node: Expression): String {
        return node.accept(this)
    }

    // 访问二元表达式
    override fun visitBinary(node: BinaryExpression): String {
        // 生成左操作数代码
        node.left.accept(this)
        // 生成右操作数代码
        node.right.accept(this)

        // 根据操作符生成相应的指令
        val instruction = when (node.operator.type) {
            TokenType.PLUS -> OpCode.ADD
            TokenType.MINUS -> OpCode.SUB
            TokenType.MULTIPLY -> OpCode.MUL
            TokenType.DIVIDE -> OpCode.DIV
            TokenType.EQUAL -> OpCode.EQUAL
            TokenType.NOT_EQUAL -> OpCode.NOT_EQUAL
            TokenType.LESS_THAN -> OpCode.LESS_THAN
            TokenType.GREATER_THAN -> OpCode.GREATER_THAN
            TokenType.LESS_EQUAL -> OpCode.LESS_EQUAL
            TokenType.GREATER_EQUAL -> OpCode.GREATER_EQUAL
            else -> throw IllegalArgumentException("Unsupported binary operator: ${node.operator.type}")
        }

        vm.addInstruction(Instruction(instruction))
        return ""
    }

    // 访问一元表达式
    override fun visitUnary(node: UnaryExpression): String {
        node.operand.accept(this)

        val instruction = when (node.operator.type) {
            TokenType.MINUS -> OpCode.NEG
            TokenType.LOGICAL_NOT -> OpCode.NOT
            else -> throw IllegalArgumentException("Unsupported unary operator: ${node.operator.type}")
        }

        vm.addInstruction(Instruction(instruction))
        return ""
    }

    // 访问赋值表达式
    override fun visitAssignment(node: AssignmentExpression): String {
        node.value.accept(this)
        
        when (node.target) {
            is Identifier -> vm.addInstruction(Instruction(OpCode.STORE, node.target.name))
            else -> throw IllegalArgumentException("Invalid assignment target")
        }
        return ""
    }

    // 访问函数调用
    override fun visitCall(node: CallExpression): String {
        // 计算并压入参数
        for (arg in node.arguments) {
            arg.accept(this)
        }

        // 获取函数名
        val functionName = when (val callee = node.callee) {
            is Identifier -> callee.name
            else -> throw IllegalArgumentException("Invalid function call target")
        }

        // 如果是内置函数，使用特殊指令
        when (functionName) {
            "print" -> vm.addInstruction(Instruction(OpCode.PRINT))
            else -> {
                // 获取函数标签并调用
                val functionLabel = functionLabels[functionName]
                    ?: throw IllegalArgumentException("Undefined function: $functionName")
                vm.addInstruction(Instruction(OpCode.CALL, functionLabel))
            }
        }
        return ""
    }

    // 访问标识符
    override fun visitIdentifier(node: Identifier): String {
        vm.addInstruction(Instruction(OpCode.LOAD, node.name))
        return ""
    }

    // 访问字面量
    override fun visitLiteral(node: LiteralExpression): String {
        vm.addInstruction(Instruction(OpCode.PUSH, node.value))
        return ""
    }
} 