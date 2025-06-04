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
    // 详细输出模式
    private var isVerboseMode = true

    // 设置调试模式
    fun setDebugMode(debug: Boolean) {
        isDebugMode = debug
    }

    // 设置详细输出模式
    fun setVerboseMode(verbose: Boolean) {
        isVerboseMode = verbose
    }

    // 添加指令
    fun addInstruction(instruction: Instruction) {
        if (instruction.opCode == OpCode.LABEL) {
            labelMap[instruction.operand as Int] = instructions.size
        }
        if (isVerboseMode) {
            println("添加指令: ${instruction.opCode} ${instruction.operand ?: ""}")
        }
        instructions.add(instruction)
    }

    // 执行所有指令
    fun execute() {
        if (isVerboseMode) {
            println("\n=== 开始执行程序 ===")
            println("初始内存状态: $memory")
            println("指令数量: ${instructions.size}")
        }
        
        pc = 0
        while (pc < instructions.size) {
            val instruction = instructions[pc]
            
            if (isVerboseMode) {
                println("\n执行指令[$pc]: ${instruction.opCode} ${instruction.operand ?: ""}")
                println("当前栈: $stack")
                if (memory.isNotEmpty()) {
                    println("当前内存: $memory")
                }
            }
            
            executeInstruction(instruction)
            
            // 检查栈大小
            if (stack.size > 1000) {
                throw RuntimeException("栈溢出: 栈大小超过1000")
            }
            
            // 检查调用栈大小
            if (callStack.size > 100) {
                throw RuntimeException("调用栈溢出: 可能存在无限递归")
            }
            
            pc++
        }
        
        if (isVerboseMode) {
            println("\n=== 程序执行完成 ===")
            println("最终栈状态: $stack")
            println("最终内存状态: $memory")
        }
    }

    // 执行单条指令
    private fun executeInstruction(instruction: Instruction) {
        try {
            when (instruction.opCode) {
                OpCode.PUSH -> {
                    stack.add(instruction.operand!!)
                    if (isVerboseMode) println("压入值: ${instruction.operand}")
                }
                OpCode.POP -> {
                    if (stack.isNotEmpty()) {
                        val value = stack.removeAt(stack.size - 1)
                        if (isVerboseMode) println("弹出值: $value")
                    }
                }
                OpCode.ADD -> executeBinaryOp("加法") { a, b -> a + b }
                OpCode.SUB -> executeBinaryOp("减法") { a, b -> a - b }
                OpCode.MUL -> executeBinaryOp("乘法") { a, b -> a * b }
                OpCode.DIV -> executeBinaryOp("除法") { a, b -> 
                    if (b == 0.0) throw ArithmeticException("除数不能为零")
                    a / b 
                }
                OpCode.NEG -> executeUnaryOp("取负") { -it }
                OpCode.NOT -> {
                    if (stack.isNotEmpty()) {
                        val a = stack.removeAt(stack.size - 1)
                        val result = !(a as Boolean)
                        stack.add(result)
                        if (isVerboseMode) println("逻辑非: $a -> $result")
                    }
                }
                OpCode.EQUAL -> executeBinaryOp("相等比较") { a, b -> a == b }
                OpCode.NOT_EQUAL -> executeBinaryOp("不等比较") { a, b -> a != b }
                OpCode.LESS_THAN -> executeBinaryOp("小于比较") { a, b -> a < b }
                OpCode.GREATER_THAN -> executeBinaryOp("大于比较") { a, b -> a > b }
                OpCode.LESS_EQUAL -> executeBinaryOp("小于等于比较") { a, b -> a <= b }
                OpCode.GREATER_EQUAL -> executeBinaryOp("大于等于比较") { a, b -> a >= b }
                OpCode.STORE -> {
                    if (stack.isNotEmpty()) {
                        val value = stack.last()
                        val name = instruction.operand as String
                        memory[name] = value
                        if (isVerboseMode) println("存储变量: $name = $value")
                    }
                }
                OpCode.LOAD -> {
                    val name = instruction.operand as String
                    val value = memory[name] ?: throw RuntimeException("未找到变量: $name")
                    stack.add(value)
                    if (isVerboseMode) println("加载变量: $name = $value")
                }
                OpCode.PRINT -> {
                    if (stack.isNotEmpty()) {
                        val value = stack.removeAt(stack.size - 1)
                        if (isVerboseMode) println("打印值: $value")
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
                        if (isVerboseMode) println("格式化打印: format=\"$format\", args=$args")
                        try {
                            val processedArgs = args.map { arg ->
                                when (arg) {
                                    is Double -> if (arg == arg.toInt().toDouble()) arg.toInt() else arg
                                    else -> arg
                                }
                            }
                            print(String.format(format, *processedArgs.toTypedArray()))
                        } catch (e: Exception) {
                            throw RuntimeException("Printf格式化错误: ${e.message}")
                        }
                    }
                }
                OpCode.LABEL -> {
                    if (isVerboseMode) println("标签定义: ${instruction.operand}")
                }
                OpCode.JMP -> {
                    val targetLabel = instruction.operand as Int
                    val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("无效的跳转目标: $targetLabel")
                    if (isVerboseMode) println("无条件跳转到标签 $targetLabel (指令索引: $targetIndex)")
                    pc = targetIndex - 1
                }
                OpCode.JZ -> {
                    if (stack.isNotEmpty()) {
                        val condition = stack.removeAt(stack.size - 1)
                        if (isVerboseMode) println("条件判断: $condition")
                        if (condition == false || condition == 0 || condition == 0.0) {
                            val targetLabel = instruction.operand as Int
                            val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("无效的跳转目标: $targetLabel")
                            if (isVerboseMode) println("条件为假，跳转到标签 $targetLabel (指令索引: $targetIndex)")
                            pc = targetIndex - 1
                        } else {
                            if (isVerboseMode) println("条件为真，继续执行")
                        }
                    }
                }
                OpCode.CALL -> {
                    val targetLabel = instruction.operand as Int
                    val targetIndex = labelMap[targetLabel] ?: throw RuntimeException("无效的函数标签: $targetLabel")
                    callStack.add(Frame(pc + 1, basePointer))
                    basePointer = stack.size
                    if (isVerboseMode) println("调用函数: 标签 $targetLabel (指令索引: $targetIndex), 保存返回地址: ${pc + 1}")
                    pc = targetIndex - 1
                }
                OpCode.RETURN -> {
                    val returnValue = if (stack.isNotEmpty()) stack.last() else null
                    if (callStack.isNotEmpty()) {
                        val frame = callStack.removeAt(callStack.size - 1)
                        pc = frame.returnAddress
                        if (isVerboseMode) println("函数返回: 返回值=$returnValue, 返回地址=${frame.returnAddress}")
                        while (stack.size > basePointer) {
                            stack.removeAt(stack.size - 1)
                        }
                        basePointer = frame.basePointer
                        if (returnValue != null) {
                            stack.add(returnValue)
                        }
                    } else {
                        if (isVerboseMode) println("程序返回: 返回值=$returnValue")
                        pc = instructions.size
                    }
                }
                OpCode.HALT -> {
                    if (isVerboseMode) println("程序终止")
                    pc = instructions.size
                }
            }
        } catch (e: Exception) {
            throw RuntimeException("执行指令 ${instruction.opCode} 时出错: ${e.message}")
        }
    }

    // 执行二元操作
    private inline fun executeBinaryOp(opName: String, operation: (Double, Double) -> Any) {
        if (stack.size >= 2) {
            val b = (stack.removeAt(stack.size - 1) as Number).toDouble()
            val a = (stack.removeAt(stack.size - 1) as Number).toDouble()
            val result = operation(a, b)
            stack.add(result)
            if (isVerboseMode) println("$opName: $a ${opName} $b = $result")
        }
    }

    // 执行一元操作
    private inline fun executeUnaryOp(opName: String, operation: (Double) -> Double) {
        if (stack.isNotEmpty()) {
            val a = (stack.removeAt(stack.size - 1) as Number).toDouble()
            val result = operation(a)
            stack.add(result)
            if (isVerboseMode) println("$opName: $a -> $result")
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