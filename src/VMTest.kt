import java.io.File

fun main() {
    // 创建词法分析器、语法分析器和虚拟机实例
    val testProgram = """
        int main() {
            int x = 10;
            int y = 20;
            int result = x + y;
            printf("%d\n", result);
            return 0;
        }
    """.trimIndent()

    try {
        // 1. 词法分析
        val lexer = CLexer(testProgram)
        val tokens = lexer.tokenize()
        println("词法分析完成，生成的Token：")
        tokens.forEach { println(it.toOutputString()) }
        println()

        // 2. 语法分析
        val parser = CParser(tokens)
        val ast = parser.parse()
        println("语法分析完成，生成的AST：")
        val astPrinter = ASTPrintVisitor()
        println(ast.accept(astPrinter))
        println()

        // 3. 语义分析
        val semanticAnalyzer = SemanticAnalyzer()
        ast.accept(semanticAnalyzer)
        println("语义分析完成")
        println()

        // 4. 代码生成和虚拟机执行
        val vm = VirtualMachine()
        val codeGenerator = CodeGenerator(vm)
        
        println("开始生成虚拟机指令...")
        ast.accept(codeGenerator)
        
        println("生成的虚拟机指令：")
        vm.getInstructions().forEachIndexed { index, instruction ->
            println("$index: ${instruction.opCode} ${instruction.operand ?: ""}")
        }
        println()

        println("开始执行程序...")
        vm.execute()
        println("程序执行完成")
        
        // 打印最终的变量状态
        println("\n最终变量状态：")
        println("x = ${vm.getVariable("x")}")
        println("y = ${vm.getVariable("y")}")
        println("result = ${vm.getVariable("result")}")

    } catch (e: Exception) {
        println("错误：${e.message}")
        e.printStackTrace()
    }
} 