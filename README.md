# Kotlin Virtual Machine

一个用 Kotlin 实现的简单但功能完整的虚拟机，支持基本的算术运算、控制流、函数调用等功能。

## 特性

- 基于栈的虚拟机架构
- 支持变量存储和访问
- 完整的算术和逻辑运算支持
- 控制流（条件判断、循环）
- 函数调用和返回
- 详细的执行日志输出
- 内置安全限制，防止栈溢出和无限递归

## 指令集

### 栈操作
- `PUSH value` - 将值压入栈
- `POP` - 弹出栈顶值

### 算术运算
- `ADD` - 加法 (a + b)
- `SUB` - 减法 (a - b)
- `MUL` - 乘法 (a * b)
- `DIV` - 除法 (a / b)
- `NEG` - 取负 (-a)

### 逻辑运算
- `NOT` - 逻辑非 (!a)

### 比较运算
- `EQUAL` - 相等比较 (a == b)
- `NOT_EQUAL` - 不等比较 (a != b)
- `LESS_THAN` - 小于比较 (a < b)
- `GREATER_THAN` - 大于比较 (a > b)
- `LESS_EQUAL` - 小于等于比较 (a <= b)
- `GREATER_EQUAL` - 大于等于比较 (a >= b)

### 变量操作
- `STORE name` - 存储变量
- `LOAD name` - 加载变量

### 输出操作
- `PRINT` - 打印栈顶值
- `PRINTF` - 格式化打印

### 控制流
- `LABEL label` - 定义标签
- `JMP label` - 无条件跳转
- `JZ label` - 条件跳转（栈顶为0/false时跳转）

### 函数操作
- `CALL label` - 函数调用
- `RETURN` - 函数返回

### 程序控制
- `HALT` - 停止执行

## 安全限制

- 最大栈深度：1000
- 最大调用栈深度：100

## 使用示例

### 基本算术运算
```kotlin
// 计算 1 + 2 并打印结果
vm.addInstruction(Instruction(OpCode.PUSH, 1))
vm.addInstruction(Instruction(OpCode.PUSH, 2))
vm.addInstruction(Instruction(OpCode.ADD))
vm.addInstruction(Instruction(OpCode.PRINT))
```

### 变量使用
```kotlin
// 存储和加载变量
vm.addInstruction(Instruction(OpCode.PUSH, 10))
vm.addInstruction(Instruction(OpCode.STORE, "x"))
vm.addInstruction(Instruction(OpCode.LOAD, "x"))
```

### 条件语句
```kotlin
// if (x > 0) print(x)
val endLabel = newLabel()
vm.addInstruction(Instruction(OpCode.LOAD, "x"))
vm.addInstruction(Instruction(OpCode.PUSH, 0))
vm.addInstruction(Instruction(OpCode.LESS_EQUAL))
vm.addInstruction(Instruction(OpCode.JZ, endLabel))
vm.addInstruction(Instruction(OpCode.LOAD, "x"))
vm.addInstruction(Instruction(OpCode.PRINT))
vm.addInstruction(Instruction(OpCode.LABEL, endLabel))
```

### 函数定义和调用
```kotlin
// 定义函数
vm.addInstruction(Instruction(OpCode.LABEL, functionLabel))
// ... 函数体指令 ...
vm.addInstruction(Instruction(OpCode.RETURN))

// 调用函数
vm.addInstruction(Instruction(OpCode.CALL, functionLabel))
```

## 调试功能

虚拟机支持详细的执行日志输出，可以通过 `setVerboseMode` 控制：

```kotlin
vm.setVerboseMode(true) // 启用详细输出
```

输出示例：
