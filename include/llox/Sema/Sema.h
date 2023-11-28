#ifndef LLOX_SEMA_SEMA_H
#define LLOX_SEMA_SEMA_H

#include <memory>

#include "llox/AST/AST.h"
#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llox/Sema/Scope.h"
#include "llox/Sema/TypeChecker.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

namespace llox {

class TypeChecker;

class Sema {
   Scope *GlobalScope;
   Scope *CurScope;
   Stmt *CurDecl;
   TypeChecker TyChecker;

   TypeDecl *IntType;
   TypeDecl *DoubleType;
   TypeDecl *BoolType;
   TypeDecl *StringType;
   BoolLiteral *TrueLiteral;
   BoolLiteral *FalseLiteral;

public:
   void enterScope(Stmt *D);
   void leaveScope();

public:
   Sema();

   TypeDecl *getBool();
   TypeDecl *getInt();
   TypeDecl *getDouble();

   CompilationUnitDecl *actOnCompilationUnit(StringRef Name);

   void actOnCompilationUnit(CompilationUnitDecl *CompUnit, StmtList Stmts);
   void actOnIfStmt(StmtList &Stmts, Expr *Cond, StmtList &IfElseStmts);
   void actOnWhileStmt(StmtList &Stmts, Expr *Cond, StmtList &WhileStmts);
   void actOnReturnStmt(StmtList &Stmts, Expr *E);
   BlockStmt *actOnBlockStmt(StmtList &Stmts);
   void actOnBlockStmt(BlockStmt *Block, StmtList &BlockStmts);
   void actOnExprStmt(StmtList &Stmts, Expr *E);
   void *actOnAssignmentInit(StmtList &Decls, Expr *E);
   StructTypeDecl *actOnStructDecl(StmtList &Decls, Identifier &Id);
   void actOnStructFields(StructTypeDecl *StructD, StmtList &FieldStmts);

   Expr *actOnAssignmentExpr(Expr *Left, Expr *Right);
   FunctionDecl *actOnFunctionDecl(SMLoc Loc, StringRef Name);
   void actOnFunctionParamList(FunctionDecl *FunDecl, ParameterList &Params, Decl *RetTy);
   void actOnFunctionBlock(StmtList &Decls, FunctionDecl *FunDecl, StmtList &FunStmts);
   void actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, StmtList &ParTypes, llvm::SmallVector<bool, 8> &VarFlags);
   Decl *actOnNameLookup(Stmt *Prev, SMLoc Loc, StringRef Name);
   bool actOnVariableDecl(StmtList &Decls, Identifier Id, Decl *D);
   void actOnField(StmtList &Stmts, Identifier &Id, Decl *Ty);
   Expr *actOnDoubleLiteral(SMLoc Loc, StringRef Literal);
   Expr *actOnIntLiteral(SMLoc Loc, StringRef Literal);
   Expr *actOnBoolLiteral(tok::TokenKind K);
   Expr *actOnStringLiteral(SMLoc Loc, StringRef Literal);
   Expr *actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op);
   Expr *actOnPrefixExpr(Expr *E, const OperatorInfo &Op);
   Expr *actOnFunctionCallExpr(Identifier &FunId, ExprList &ParamExprs);

   void actOnSelectorList(Expr *O, SelectorList &SL);
   TypeDecl *actOnFieldSelector(Stmt *O, SelectorList &SelList, StringRef Name);
   TypeDecl *actOnIndexSelector(Stmt *O, SelectorList &SelList, Expr *IdxE);
   Expr *actOnObjectExpr(Identifier &Id);

   TypeDecl *actOnArrayTypeDecl(Decl *BaseTy, Expr *E);
};

} // namespace llox

#endif