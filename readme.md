## llox = LLVM + lox
Lox lang is a lang from Crafting Interpreters.

## TODO:

1. Lexer - minimal done
2. Parser - minimal done
3. Sema - for exprs, decls
4. AST Printer - crutchy but enough
5. Error Printer - after basic codegen ready
6. Translate language to IR - basic
---------------------------------------------
7. Passes, ... 

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

var arr: int[5]; // we'll also add static arrays

class Point {
  fn init(x: double, y: double) : nil {
    var this.x = x;
    var this.y = y;
  }

  fn getDistanceSquared(B: Point) : double {
    dx: double = this.x - B.x;
    dy: double = this.y - B.y;
    dist: double = dx * dx + dy * dy;
    return dist;
  }
};

fun main() -> int {
  P1: Point = Point(2, 2);
  P2: Point = Point(0, 0);
  print(P1.getDisanceSquared(P2))
  return 0;
}
```

```
var a, b: int = 5, 6; // DO WE NEED THIS?
```

Grammar from original language:
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

