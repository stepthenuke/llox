#ifndef LLOX_PARSER_PARSER_H
#define LLOX_PARSER_PARSER_H

#include "llox/AST/AST.h"
#include "llox/Basic/TokenKinds.h"
#include "llox/Lexer/Lexer.h"
#include "llox/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace llox {

class Parser {
   Token CurTok;
   Lexer &Lex;
   Sema &Sem;

   using OperatorPrec = op::OperatorPrec;

public:
   Parser(Lexer &L, Sema &S);

   CompilationUnitDecl *parse();

public:
   void nextToken();
   
   bool peekToken(tok::TokenKind Kind, int n);
   bool consumeToken(tok::TokenKind Kind);
   bool consumeToken(std::initializer_list<tok::TokenKind> &&Kinds);

   OperatorPrec getBinOperatorPrec();
   OperatorPrec getUnOperatorPrec();

   bool validStmtDelimiter(StmtList &Stmts);

   bool parseCompilationUnit(CompilationUnitDecl *&CompUnit);
   bool parseDecl(StmtList &Decls);
   bool parseVariableDecl(StmtList &Decls);
   bool parseTypeIdent(Decl *&D);
   bool parseFunctionParameter(IdentList &ParIds, StmtList &ParTypes);
   bool parseFunctionParameterList(ParameterList &Params);
   bool parseBlock(StmtList &Decls, StmtList &Stmts);
   bool parseFunctionDecl(StmtList &Decls);
   bool parseExprList(ExprList &Exprs);
   bool parseIdentifierExpr(Expr *&E);
   bool parseStringLiteral(Expr *&E);
   bool parseParenExpr(Expr *&E);
   bool parseDoubleLiteral(Expr *&E);
   bool parseBoolLiteral(Expr *&E);
   bool parsePrimary(Expr *&E);
   bool parseExpr(Expr *&E);
   bool parseInfixExpr(OperatorPrec LeftPrec, Expr *&Left);
   bool parsePrefixExpr(Expr *&E);
   
   bool parseStmtList(StmtList &Stmts);
   bool parseStmt(StmtList &Stmts);
   bool parseIfStmt(StmtList &Stmts);
   bool parseWhileStmt(StmtList &Stmts);
   bool parseReturnStmt(StmtList &Stmts);
   bool parseForStmt(StmtList &Stmts);
   bool parseExprStmt(StmtList &Stmts);
   bool parseBracedStmts(StmtList &Stmts);
   bool parseBlock(StmtList &Stmts);
};

} // namespace llox

#endif