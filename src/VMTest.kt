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
        println("=== 编译开始 ===")
        
        // 1. 词法分析
        val lexer = CLexer(testProgram)
        val tokens = lexer.tokenize()
        println("✓ 词法分析完成")

        // 2. 语法分析
        val parser = CParser(tokens)
        val ast = parser.parse()
        println("✓ 语法分析完成")

        // 3. 语义分析
        val semanticAnalyzer = SemanticAnalyzer()
        ast.accept(semanticAnalyzer)
        println("✓ 语义分析完成")

        // 4. 代码生成和虚拟机执行
        val vm = VirtualMachine()
        val codeGenerator = CodeGenerator(vm)
        
        println("\n=== 程序输出 ===")
        ast.accept(codeGenerator)
        vm.execute()
        
        // 打印最终的变量状态
        println("\n=== 执行结果 ===")
        println("x = ${vm.getVariable("x")}")
        println("y = ${vm.getVariable("y")}")
        println("result = ${vm.getVariable("result")}")
        println("\n=== 编译执行完成 ===")

    } catch (e: Exception) {
        println("\n=== 错误信息 ===")
        println("错误：${e.message}")
        e.printStackTrace()
    }
} 