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


## Current progress
```
fun gcd(a: double, b: double) : double {
   if (a == 0) {
      return b;
   }
   while (b != 0) {
      if (a > b) {
         a = a - b;
      }
      else {
         b = b - a;
      }
   }
   return a;
}

define double @gcd(double %a, double %b) {
entry:
  %b2 = alloca double, align 8
  %a1 = alloca double, align 8
  store double %a, ptr %a1, align 8
  store double %b, ptr %b2, align 8
  %0 = load double, ptr %a1, align 8
  %1 = fcmp oeq double %0, 0.000000e+00
  br i1 %1, label %if.body, label %cont.if

if.body:                                          ; preds = %entry
  %2 = load double, ptr %b2, align 8
  ret double %2

cont.if:                                          ; preds = %entry
  br label %while.cond

while.cond:                                       ; preds = %cont.if4, %cont.if
  %3 = load double, ptr %b2, align 8
  %4 = fcmp one double %3, 0.000000e+00
  br i1 %4, label %while.body, label %cont.while

while.body:                                       ; preds = %while.cond
  %5 = load double, ptr %a1, align 8
  %6 = load double, ptr %b2, align 8
  %7 = fcmp ogt double %5, %6
  br i1 %7, label %if.body3, label %else.body

cont.while:                                       ; preds = %while.cond
  %8 = load double, ptr %a1, align 8
  ret double %8

if.body3:                                         ; preds = %while.body
  %9 = load double, ptr %a1, align 8
  %10 = load double, ptr %b2, align 8
  %11 = fsub double %9, %10
  store ptr %a1, double %11, align 8
  br label %cont.if4

else.body:                                        ; preds = %while.body
  %12 = load double, ptr %b2, align 8
  %13 = load double, ptr %a1, align 8
  %14 = fsub double %12, %13
  store ptr %b2, double %14, align 8
  br label %cont.if4

cont.if4:                                         ; preds = %else.body, %if.body3
  br label %while.cond
}
```