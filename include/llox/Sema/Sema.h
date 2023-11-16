#ifndef LLOX_SEMA_SEMA_H
#define LLOX_SEMA_SEMA_H

#include <memory>

#include "llox/AST/AST.h"
#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llox/Sema/Scope.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

namespace llox {

class Sema {
   Scope *CurScope;
   Stmt *CurDecl;

   TypeDecl *DoubleType;
   TypeDecl *BoolType;
   BoolLiteral *TrueLiteral;
   BoolLiteral *FalseLiteral;

   bool checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty);

public:
   void enterScope(Stmt *D);
   void leaveScope();

public:
   Sema();

   CompilationUnitDecl *actOnCompilationUnit(StringRef Name);

   void actOnCompilationUnit(CompilationUnitDecl *CompUnit, StmtList Stmts);
   void actOnIfStmt(StmtList &Stmts, Expr *Cond, StmtList &IfElseStmts);
   void actOnWhileStmt(StmtList &Stmts, Expr *Cond, StmtList &WhileStmts);
   void actOnReturnStmt(StmtList &Stmts, Expr *E);
   BlockStmt *actOnBlockStmt(StmtList &Stmts);
   void actOnBlockStmt(BlockStmt *Block, StmtList &BlockStmts);
   void actOnExprStmt(StmtList &Stmts, Expr *E);
   void actOnAssignmentStmt(StmtList &Decls, Expr *E);

   FunctionDecl *actOnFunctionDecl(SMLoc Loc, StringRef Name);
   void actOnFunctionParamList(FunctionDecl *FunDecl, ParameterList &Params, Decl *RetTy);
   void actOnFunctionBlock(StmtList &Decls, FunctionDecl *FunDecl, StmtList &FunStmts);
   void actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, StmtList &ParTypes);
   Decl *actOnNameLookup(Stmt *Prev, SMLoc Loc, StringRef Name);
   void actOnVariableDecl(StmtList &Decls, Identifier Id, Decl *D);
   Expr *actOnDoubleLiteral(SMLoc Loc, StringRef Literal);
   Expr *actOnBoolLiteral(tok::TokenKind K);
   Expr *actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op);
   Expr *actOnPrefixExpr(Expr *E, const OperatorInfo &Op);
   void checkFunctionParameterTypes(const ParameterList &Params, const ExprList &Exprs);
   Expr *actOnFunctionCallExpr(Identifier &FunId, ExprList &ParamExprs);
   Expr *actOnObjectExpr(Identifier &Id);
};

} // namespace llox

#endif