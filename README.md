# Chris' C Compiler

Here you find a compiler and compiler driver that accomplish three stages:

- A lexer (source code -> tokens)
- A parser (tokens -> AST)
- An assembly generator (AST -> asm)

Using an assembler and a linker, you can then turn the code into an executable.
