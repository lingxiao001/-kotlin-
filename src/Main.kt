import java.io.File

fun main() {
    println("=== C Compiler - Lexical, Syntax, and Semantic Analysis ===")
    
    // 读取源文件
    val sourceFile = File("C:\\Users\\18319\\IdeaProjects\\kotlinCompiler\\src\\lexertest.c")  // 你可以修改这个文件名来匹配你的输入文件
    if (!sourceFile.exists()) {
        println("Error: Source file not found")
        return
    }

    try {
        // 1. 词法分析
        println("\n1. Lexical Analysis...")
        val sourceCode = sourceFile.readText()
        val lexer = CLexer(sourceCode)
        val tokens = lexer.tokenize()
        println("✓ Tokens generated: ${tokens.size}")

        // 2. 语法分析
        println("\n2. Syntax Analysis...")
        val parser = CParser(tokens.filter { it.type != TokenType.COMMENT && it.type != TokenType.NEWLINE })
        val ast = parser.parse()
        println("✓ AST generated")

        // 3. 语义分析
        println("\n3. Semantic Analysis...")
        val semanticAnalyzer = SemanticAnalyzer()
        
        // 添加标准库函数到全局作用域
        semanticAnalyzer.defineStandardLibrary()
        
        try {
            semanticAnalyzer.analyze(ast)
            println("✓ Semantic analysis completed successfully")
        } catch (e: SemanticError) {
            println("Semantic Error: ${e.message} at ${e.node}")
            return
        }

        // 4. 生成输出文件
        println("\n4. Generating output files...")

        // 生成token文件
        val tokenFile = File("output_tokens.txt")
        tokenFile.writeText(generateTokenFileForParser(tokens))
        println("✓ Token file generated: ${tokenFile.name}")

        // 生成AST文件
        val astFile = File("output_ast.txt")
        astFile.writeText(generateASTFile(ast))
        println("✓ AST file generated: ${astFile.name}")

        // 生成符号表文件
        val symbolTableFile = File("output_symboltable.txt")
        symbolTableFile.writeText(generateSymbolTableFile(ast))
        println("✓ Symbol table file generated: ${symbolTableFile.name}")

        // 打印编译统计信息
        println("\n=== Compilation Statistics ===")
        println("Source lines: ${sourceCode.lines().size}")
        println("Tokens: ${tokens.size}")
        println("AST nodes: ${countASTNodes(ast)}")
        
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

// 计算AST节点数量的辅助函数
private fun countASTNodes(node: ASTNode): Int {
    var count = 1  // 当前节点
    
    when (node) {
        is Program -> count += node.declarations.sumOf { countASTNodes(it) }
        is FunctionDeclaration -> {
            count += node.parameters.size
            if (node.body != null) count += countASTNodes(node.body)
        }
        is BlockStatement -> count += node.statements.sumOf { countASTNodes(it) }
        is IfStatement -> {
            count += countASTNodes(node.condition)
            count += countASTNodes(node.thenStatement)
            if (node.elseStatement != null) count += countASTNodes(node.elseStatement)
        }
        is WhileStatement -> {
            count += countASTNodes(node.condition)
            count += countASTNodes(node.body)
        }
        is ForStatement -> {
            if (node.init != null) count += countASTNodes(node.init)
            if (node.condition != null) count += countASTNodes(node.condition)
            if (node.update != null) count += countASTNodes(node.update)
            count += countASTNodes(node.body)
        }
        is ReturnStatement -> {
            if (node.expression != null) count += countASTNodes(node.expression)
        }
        is BinaryExpression -> {
            count += countASTNodes(node.left)
            count += countASTNodes(node.right)
        }
        is UnaryExpression -> count += countASTNodes(node.operand)
        is CallExpression -> {
            count += countASTNodes(node.callee)
            count += node.arguments.sumOf { countASTNodes(it) }
        }
        is AssignmentExpression -> {
            count += countASTNodes(node.target)
            count += countASTNodes(node.value)
        }
    }
    
    return count
} 