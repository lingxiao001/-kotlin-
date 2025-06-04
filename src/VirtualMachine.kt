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

    // 添加指令
    fun addInstruction(instruction: Instruction) {
        instructions.add(instruction)
    }

    // 执行所有指令
    fun execute() {
        while (pc < instructions.size) {
            val instruction = instructions[pc]
            executeInstruction(instruction)
            pc++
        }
    }

    // 执行单条指令
    private fun executeInstruction(instruction: Instruction) {
        when (instruction.opCode) {
            OpCode.PUSH -> stack.add(instruction.operand!!)
            OpCode.POP -> stack.removeAt(stack.size - 1)
            OpCode.ADD -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() + b.toDouble())
            }
            OpCode.SUB -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() - b.toDouble())
            }
            OpCode.MUL -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() * b.toDouble())
            }
            OpCode.DIV -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                if (b.toDouble() == 0.0) {
                    throw ArithmeticException("Division by zero")
                }
                stack.add(a.toDouble() / b.toDouble())
            }
            OpCode.NEG -> {
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(-a.toDouble())
            }
            OpCode.NOT -> {
                val a = stack.removeAt(stack.size - 1)
                stack.add(!(a as Boolean))
            }
            OpCode.EQUAL -> {
                val b = stack.removeAt(stack.size - 1)
                val a = stack.removeAt(stack.size - 1)
                stack.add(a == b)
            }
            OpCode.NOT_EQUAL -> {
                val b = stack.removeAt(stack.size - 1)
                val a = stack.removeAt(stack.size - 1)
                stack.add(a != b)
            }
            OpCode.LESS_THAN -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() < b.toDouble())
            }
            OpCode.GREATER_THAN -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() > b.toDouble())
            }
            OpCode.LESS_EQUAL -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() <= b.toDouble())
            }
            OpCode.GREATER_EQUAL -> {
                val b = stack.removeAt(stack.size - 1) as Number
                val a = stack.removeAt(stack.size - 1) as Number
                stack.add(a.toDouble() >= b.toDouble())
            }
            OpCode.STORE -> {
                val value = stack.last()
                val name = instruction.operand as String
                memory[name] = value
            }
            OpCode.LOAD -> {
                val name = instruction.operand as String
                val value = memory[name] ?: throw RuntimeException("Variable $name not found")
                stack.add(value)
            }
            OpCode.PRINT -> {
                val value = stack.removeAt(stack.size - 1)
                println(value)
            }
            OpCode.JMP -> {
                pc = (instruction.operand as Int) - 1  // -1 because pc will be incremented after instruction
            }
            OpCode.JZ -> {
                val condition = stack.removeAt(stack.size - 1)
                if (condition == false || condition == 0 || condition == 0.0) {
                    pc = (instruction.operand as Int) - 1
                }
            }
            OpCode.CALL -> {
                // 保存返回地址和基址指针
                callStack.add(Frame(pc + 1, basePointer))
                basePointer = stack.size
                pc = (instruction.operand as Int) - 1
            }
            OpCode.RETURN -> {
                // 保存返回值
                val returnValue = if (stack.isNotEmpty()) stack.last() else null
                
                // 恢复调用帧
                val frame = callStack.removeAt(callStack.size - 1)
                pc = frame.returnAddress
                basePointer = frame.basePointer
                
                // 清理栈并压入返回值
                while (stack.size > basePointer) {
                    stack.removeAt(stack.size - 1)
                }
                if (returnValue != null) {
                    stack.add(returnValue)
                }
            }
            OpCode.HALT -> {
                pc = instructions.size  // Stop execution
            }
        }
    }

    // 获取栈顶值
    fun getTopValue(): Any? {
        return if (stack.isNotEmpty()) stack.last() else null
    }

    // 获取变量值
    fun getVariable(name: String): Any? {
        return memory[name]
    }

    // 清除虚拟机状态
    fun reset() {
        stack.clear()
        memory.clear()
        pc = 0
        instructions.clear()
        callStack.clear()
        basePointer = 0
    }

    // 获取当前指令
    fun getCurrentInstruction(): Instruction? {
        return if (pc < instructions.size) instructions[pc] else null
    }

    // 获取所有指令
    fun getInstructions(): List<Instruction> = instructions.toList()

    // 获取调用栈信息
    fun getCallStackTrace(): List<Frame> = callStack.toList()
} 