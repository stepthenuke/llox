## llox = LLVM + lox

Lox lang is a lang from crafting interpreters.

Grammar:
```
program     -> declaration* eof

declaration -> varDecl
            | statement

varDecl     -> "var" IDENTIFIER ( "=" expression )? ";"

statement   -> exprStmt
            | forStmt
            | ifStmt
            | printStmt
            | whileStmt
            | blockStmt

exprStmt    -> expression ";"

forStmt     -> "for" "(" (varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement

ifStmt      -> "if" "(" expression ")" statement ( "else" statement )?

blockStmt   -> "{" declaration* "}"

whileStmt   -> "while" "(" expression ")" statement

printStmt   -> "print" expression ";"

expression  -> assignment

assignment  -> identifier ( "=" assignment )?
            | logic_or

logic_or    -> logic_and ( "or " logic_and )*

logic_and   -> equality ( "and" equality )*

equality    -> comparison ( ( "!=" | "==" ) comparison )*

comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )*

term        -> factor ( ( "-" | "+" ) factor )*

factor      -> unary ( ( "/" | "*" ) unary )*

unary       -> ( "!" | "-" ) unary
            | primary

primary     -> NUMBER | STRING
            | "false" | "true" | "nil"
            | "(" expression ")"
            | IDENTIFIER
```

TODO:

1. Lexer
2. Parser
3. Translate language to IR
4. ... 

Firstly I implement all basics in one file -> then split to normal project config
