# Kotlin Compiler Project

A custom compiler implementation written in Kotlin, featuring robust semantic analysis capabilities.

## Project Overview

This project implements a compiler with a focus on strong semantic analysis. The compiler performs type checking, scope analysis, and various semantic validations to ensure code correctness before compilation.

### Key Features

- **Type System**
  - Static type checking
  - Type compatibility verification
  - Support for basic types (int, float, bool, string)
  - Type inference for expressions

- **Scope Management**
  - Hierarchical scope system
  - Variable and function symbol management
  - Prevention of duplicate declarations
  - Proper variable initialization tracking

- **Function Analysis**
  - Function declaration validation
  - Parameter type checking
  - Return type verification
  - Built-in function support (print, printf)

- **Control Flow Analysis**
  - Validation of if-else statements
  - While loop condition checking
  - For loop semantic validation
  - Return statement verification

## Getting Started

### Prerequisites

- JDK 8 or higher
- Kotlin compiler

### Building the Project

1. Clone the repository:
```bash
git clone [your-repository-url]
cd kotlinCompiler
```

2. Build the project:
```bash
./gradlew build
```

## Usage

### Basic Compilation

To compile a source file:

```bash
java -jar kotlinCompiler.jar [source-file]
```

### Semantic Analysis

The semantic analyzer performs the following checks:

1. Variable declarations and initializations
2. Function declarations and calls
3. Type compatibility in expressions
4. Scope rules and symbol resolution
5. Control flow statement validation

### Example

```kotlin
// Example source code
function main(): int {
    var x: int = 10
    var y: float = 20.5
    
    if (x < y) {
        print("x is less than y")
    }
    
    return 0
}
```

## Error Handling

The compiler provides detailed semantic error messages, including:

- Undefined variable references
- Type mismatch errors
- Invalid function calls
- Duplicate declarations
- Uninitialized variable usage

## Project Structure

- `src/`
  - `SemanticAnalysis.kt` - Main semantic analyzer implementation
  - [Other source files...]

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

