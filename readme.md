## llox = LLVM + lox
Lox lang is a lang from Crafting Interpreters.

## TODO:

1. Lexer - minimal done
2. Parser - minimal done
3. Sema - for exprs, decls
4. AST Printer - crutchy but enough
5. Error Printer - TOP PRIORITY
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

Current Progress:
```
foo(1, 2);

if (5 + foo(1, 2) * 5) {
   5 + foo(1, 2);
}
else {
   return 10;
}

foo((1 + -2) * 5 - 100 * (1 / 2 + -1), 2);
343 + 123;

434 + 34 * (3 * 4 - 34 / 23);

fun foo(a: double, b: double) : double {
   var c : double;
   343 * 13 - 1 / 34;
}
var a : double = 50;

GOES TO:
------------------------------

FunctionCallExpr TEST_FUNC <double> 
   DoubleLiteral 1
   DoubleLiteral 2
IfStmt
   InfixExpr <double> plus:
      DoubleLiteral 5
      InfixExpr <double> star:
         FunctionCallExpr TEST_FUNC <double> 
            DoubleLiteral 1
            DoubleLiteral 2
         DoubleLiteral 5
------------------------------
   Block
            InfixExpr <double> plus:
         DoubleLiteral 5
         FunctionCallExpr TEST_FUNC <double> 
            DoubleLiteral 1
            DoubleLiteral 2
   Block
      ReturnStmt
         DoubleLiteral 10
      FunctionCallExpr TEST_FUNC <double> 
   InfixExpr <double> minus:
      InfixExpr <double> star:
         InfixExpr <double> plus:
            DoubleLiteral 1
            PrefixExpr <double> minus:
               DoubleLiteral 2
         DoubleLiteral 5
      InfixExpr <double> star:
         DoubleLiteral 100
         InfixExpr <double> plus:
            InfixExpr <double> slash:
               DoubleLiteral 1
               DoubleLiteral 2
            PrefixExpr <double> minus:
               DoubleLiteral 1
   DoubleLiteral 2
InfixExpr <double> plus:
   DoubleLiteral 343
   DoubleLiteral 123
InfixExpr <double> plus:
   DoubleLiteral 434
   InfixExpr <double> star:
      DoubleLiteral 34
      InfixExpr <double> minus:
         InfixExpr <double> star:
            DoubleLiteral 3
            DoubleLiteral 4
         InfixExpr <double> slash:
            DoubleLiteral 34
            DoubleLiteral 23
FunctionDecl foo <double> 
------------------------------
   ParameterDecl a <double> 
   ParameterDecl b <double> 
------------------------------
   Block
      VariableDecl c <double> 
            InfixExpr <double> minus:
         InfixExpr <double> star:
            DoubleLiteral 343
            DoubleLiteral 13
         InfixExpr <double> slash:
            DoubleLiteral 1
            DoubleLiteral 34
VariableDecl a <double>
```

