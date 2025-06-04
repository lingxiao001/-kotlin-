// 指令集定义
enum class OpCode {
    PUSH,       // 将值压入栈
    POP,        // 弹出栈顶值
    ADD,        // 加法
    SUB,        // 减法
    MUL,        // 乘法
    DIV,        // 除法
    NEG,        // 取负
    NOT,        // 逻辑非
    EQUAL,      // 相等比较
    NOT_EQUAL,  // 不相等比较
    LESS_THAN,  // 小于比较
    GREATER_THAN, // 大于比较
    LESS_EQUAL, // 小于等于比较
    GREATER_EQUAL, // 大于等于比较
    STORE,      // 存储变量
    LOAD,       // 加载变量
    PRINT,      // 打印
    PRINTF,     // 格式化打印
    LABEL,      // 标签定义
    JMP,        // 无条件跳转
    JZ,         // 如果为零则跳转
    CALL,       // 函数调用
    RETURN,     // 函数返回
    HALT        // 停止执行
}

// 指令类
data class Instruction(
    val opCode: OpCode,
    val operand: Any? = null
)

// 函数帧
data class Frame(
    val returnAddress: Int,
    val basePointer: Int
)

// 虚拟机类
class VirtualMachine {
    private val stack = mutableListOf<Any>()           // 运行时栈
    private val memory = mutableMapOf<String, Any>()   // 变量存储
    private var pc = 0                                 // 程序计数器
    private val instructions = mutableListOf<Instruction>() // 指令列表
    private val callStack = mutableListOf<Frame>()     // 调用栈
    private var basePointer = 0                        // 基址指针
    private val labelMap = mutableMapOf<Int, Int>()    // 标签到指令索引的映射

    // 调试模式标志
    private var isDebugMode = false

    // 设置调试模式
    fun setDebugMode(debug: Boolean) {
        isDebugMode = debug
    }

    // 添加指令
    fun addInstruction(instruction: Instruction) {
        // 如果是标签指令，记录标签位置
        if (instruction.opCode == OpCode.LABEL) {
            labelMap[instruction.operand as Int] = instructions.size
        }
        instructions.add(instruction)
    }

    // 执行所有指令
    fun execute() {
        pc = 0
        while (pc < instructions.size) {
            if (isDebugMode) {
                println("DEBUG: Executing instruction at $pc: ${instructions[pc].opCode}")
                println("DEBUG: Stack size: ${stack.size}")
                println("DEBUG: Call stack size: ${callStack.size}")
            }
            
            val instruction = instructions[pc]
            executeInstruction(instruction)
            
            // 检查栈大小，防止栈溢出
            if (stack.size > 1000) {
                throw RuntimeException("Stack overflow: stack size exceeds 1000")
            }
            
            // 检查调用栈大小，防止无限递归
            if (callStack.size > 100) {
                throw RuntimeException("Call stack overflow: possible infinite recursion")
            }
            
            pc++
        }
    }

    // 执行单条指令
    private fun executeInstruction(instruction: Instruction) {
        try {
            when (instruction.opCode) {
                OpCode.PUSH -> stack.add(instruction.operand!!)
                OpCode.POP -> if (stack.isNotEmpty()) stack.removeAt(stack.size - 1)
                OpCode.ADD -> executeBinaryOp { a, b -> a + b }
                OpCode.SUB -> executeBinaryOp { a, b -> a - b }
                OpCode.MUL -> executeBinaryOp { a, b -> a * b }
                OpCode.DIV -> executeBinaryOp { a, b -> 
                    if (b == 0.0) throw ArithmeticException("Division by zero")
                    a / b 
                }
                OpCode.NEG -> executeUnaryOp { -it }
                OpCode.NOT -> {
                    val a = stack.removeAt(stack.size - 1)
                    stack.add(!(a as Boolean))
                }
                OpCode.EQUAL -> executeBinaryOp { a, b -> a == b }
                OpCode.NOT_EQUAL -> executeBinaryOp { a, b -> a != b }
                OpCode.LESS_THAN -> executeBinaryOp { a, b -> a < b }
                OpCode.GREATER_THAN -> executeBinaryOp { a, b -> a > b }
                OpCode.LESS_EQUAL -> executeBinaryOp { a, b -> a <= b }
                OpCode.GREATER_EQUAL -> executeBinaryOp { a, b -> a >= b }
                OpCode.STORE -> {
                    if (stack.isNotEmpty()) {
                        val value = stack.last()
                        val name = instruction.operand as String
                        memory[name] = value
                    }
                }
                OpCode.LOAD -> {
                    val name = instruction.operand as String
                    val value = memory[name] ?: throw RuntimeException("Variable $name not found")
                    stack.add(value)
                }
                OpCode.PRINT -> {
                    if (stack.isNotEmpty()) {
                        val value = stack.removeAt(stack.size - 1)
                        when (value) {
                            is Double -> {
                                if (value == value.toInt().toDouble()) {
                                    print(value.toInt())
                                } else {
                                    print(value)
                                }
                            }
                            is String -> print(value)
                            else -> print(value)
                        }
                    }
                }
                OpCode.PRINTF -> {
                    val argCount = instruction.operand as Int
                    if (stack.size >= argCount + 1) {
                        val args = mutableListOf<Any>()
                        for (i in 0 until argCount) {
                            args.add(0, stack.removeAt(stack.size - 1))
                        }
                        val format = stack.removeAt(stack.size - 1) as String
                        try {
                            val processedArgs = args.map { arg ->
                                when (arg) {
                                    is Double -> if (arg == arg.toInt().toDouble()) arg.toInt() else arg
                                    else -> arg
                                }
                            }
                            print(String.format(format, *processedArgs.toTypedArray()))
                        } catch (e: Exception) {
                            throw RuntimeException("Printf format error: ${e.message}")
                        }
                    }
                }
                OpCode.LABEL -> { /* 标签指令不需要执行任何操作 */ }
                OpCode.JMP -> {
                    val targetLabel = instruction.operand as Int
                    val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("Invalid jump target: $targetLabel")
                    pc = targetIndex - 1  // -1 because pc will be incremented after instruction
                }
                OpCode.JZ -> {
                    if (stack.isNotEmpty()) {
                        val condition = stack.removeAt(stack.size - 1)
                        if (condition == false || condition == 0 || condition == 0.0) {
                            val targetLabel = instruction.operand as Int
                            val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("Invalid jump target: $targetLabel")
                            pc = targetIndex - 1
                        }
                    }
                }
                OpCode.CALL -> {
                    val targetLabel = instruction.operand as Int
                    val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("Invalid function label: $targetLabel")
                    callStack.add(Frame(pc + 1, basePointer))
                    basePointer = stack.size
                    pc = targetIndex - 1
                }
                OpCode.RETURN -> {
                    val returnValue = if (stack.isNotEmpty()) stack.last() else null
                    if (callStack.isNotEmpty()) {
                        val frame = callStack.removeAt(callStack.size - 1)
                        pc = frame.returnAddress
                        // 清理栈并保留返回值
                        while (stack.size > basePointer) {
                            stack.removeAt(stack.size - 1)
                        }
                        basePointer = frame.basePointer
                        if (returnValue != null) {
                            stack.add(returnValue)
                        }
                    } else {
                        pc = instructions.size  // 如果没有调用帧，结束执行
                    }
                }
                OpCode.HALT -> pc = instructions.size
            }
        } catch (e: Exception) {
            throw RuntimeException("Error executing ${instruction.opCode}: ${e.message}")
        }
    }

    // 执行二元操作
    private inline fun executeBinaryOp(operation: (Double, Double) -> Any) {
        if (stack.size >= 2) {
            val b = (stack.removeAt(stack.size - 1) as Number).toDouble()
            val a = (stack.removeAt(stack.size - 1) as Number).toDouble()
            stack.add(operation(a, b))
        }
    }

    // 执行一元操作
    private inline fun executeUnaryOp(operation: (Double) -> Double) {
        if (stack.isNotEmpty()) {
            val a = (stack.removeAt(stack.size - 1) as Number).toDouble()
            stack.add(operation(a))
        }
    }

    // 获取栈顶值
    fun getTopValue(): Any? = if (stack.isNotEmpty()) stack.last() else null

    // 获取变量值
    fun getVariable(name: String): Any? = memory[name]

    // 清除虚拟机状态
    fun reset() {
        stack.clear()
        memory.clear()
        pc = 0
        instructions.clear()
        callStack.clear()
        basePointer = 0
        labelMap.clear()
    }

    // 获取当前指令
    fun getCurrentInstruction(): Instruction? = if (pc < instructions.size) instructions[pc] else null

    // 获取所有指令
    fun getInstructions(): List<Instruction> = instructions.toList()

    // 获取调用栈信息
    fun getCallStackTrace(): List<Frame> = callStack.toList()
} 