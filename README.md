# Chris' C Compiler

An attempt to write an (educational) C compiler. Provides a Lexer, Parser and a Code Generator (x86\_64).

The compiler driver uses GCC's preprocessor and assembler/linker, to help turning the code into an executable.

Supports:
- Return
- Unary operators (!, ~, -)
- Binary arithmetic and logical operators (+, -, \*, /, %, <<, >>, &, |)
- Logical and relational operators (&&, ||, <, >, <=, >=, !=, ==)
- ~~Local variables~~
- ~~Conditions~~
