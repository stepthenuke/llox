## llox = LLVM + lox
Lox lang is a lang from Crafting Interpreters.

## TODO:

1. Lexer - minimal done
2. Parser - minimal done
3. Sema - for exprs, decls
4. AST Printer
5. Error Printer
------------------------
3. Translate language to IR - .
4. ... 

## Build
```
cd llox
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_DIR=../folder_with_llvm_build/lib/cmake/llvm \ 
-DCMAKE_INSTALL_PREFIX=../llox_install_folder ..

make
make install
```

I want to make it static -> we need to change grammar a bit. Add something like:
```
var a: int = 5;
fun foo(a: int, b: int) : int {
   return a + b;
}
```

```
var a, b: int = 5, 6; // DO WE NEED THIS?
```

Grammar:
```
program        → declaration* EOF ;

declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;

classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
                 "{" function* "}" ;
funDecl        → "fun" function ;
varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

exprStmt       → expression ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           expression? ")" statement ;
ifStmt         → "if" "(" expression ")" statement
                 ( "else" statement )? ;
printStmt      → "print" expression ";" ;
returnStmt     → "return" expression? ";" ;
whileStmt      → "while" "(" expression ")" statement ;
block          → "{" declaration* "}" ;

unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;

function       → IDENTIFIER "(" parameters? ")" : type block ;
parameters     → IDENTIFIER : type ( "," IDENTIFIER : type )* ;
arguments      → expression ( "," expression )* ;

expression     → assignment ;

assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;

logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;


NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
STRING         → "\"" <any char except "\"">* "\"" ;
IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT          → "0" ... "9" ;
```



