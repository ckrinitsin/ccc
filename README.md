# Chris' C Compiler

An attempt to write an (educational) C(17) compiler. Provides a Lexer, Parser and a Code Generator (x86\_64).

The compiler driver uses GCC's preprocessor and assembler/linker, to help turning the code into an executable.

### Supports:

- Return
- Unary operators (!, ~, -)
- Binary arithmetic and logical operators (+, -, \*, /, %, <<, >>, &, |)
- Logical and relational operators (&&, ||, <, >, <=, >=, !=, ==)
- Local variables (int)
- Compound assignments and post/pre (inc/dec)rement (+=, /=, <<, ... and ++\<var\>, \<var\>-- ...) 
- If Statements (and goto + labeled statements)
- Compound Statements
- Loops and Switch Statements
- Function calls
- File scope
- Long type
