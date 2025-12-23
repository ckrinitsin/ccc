# Chris' C Compiler

An attempt to write an (educational) C compiler. Provides a Lexer, Parser and a Code Generator (x86\_64).

The compiler driver uses GCC's preprocessor and assembler/linker, to help turning the code into an executable.

Supports:
- Return
- Unary operators (!, ~, -)
- Binary arithmetic and logical operators (+, -, \*, /, %, <<, >>, &, |)
- Logical and relational operators (&&, ||, <, >, <=, >=, !=, ==)
- Local variables (int)
- Compound assignments and post/pre (inc/dec)rement (+=, /=, <<, ... and ++\<var\>, \<var\>-- ...) 
- If Statements (and goto + labeled statements)
- ~~Compound Statements~~

Here is an example of now compilable c code:
```
int main(void) {
    int a = 2 >> 1;
    int b = 1;
    b += 1;
    int c = a++;
    int d = --b;
    return (a == 2 && b == 1 && c == 1 && d == 1); // returns 1
}
```
