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
   Decl *CurDecl;

   TypeDecl *DoubleType;
   TypeDecl *BoolType;
   BoolLiteral *TrueLiteral;
   BoolLiteral *FalseLiteral;

   bool checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty);
public:
   void enterScope(Decl *D);
   void leaveScope();

public:
   Sema();
   
   FunctionDecl *actOnFunctionDecl(SMLoc Loc, StringRef Name);
   void actOnFunctionParamList(FunctionDecl *FunDecl, ParameterList &Params, Decl *RetTy);
   void actOnFunctionBlock(DeclList &Decls, FunctionDecl *FunDecl, DeclList &FunDecls, StmtList &FunStmts);
   void actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, DeclList &ParTypes);
   Decl *actOnNameLookup(Decl *Prev, SMLoc Loc, StringRef Name);
   void actOnVariableDecl(DeclList &Decls, Identifier Id, Decl *D);
   Expr *actOnDoubleLiteral(SMLoc Loc, StringRef Literal);
   Expr *actOnBoolLiteral(tok::TokenKind K);
   Expr *actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op);
   Expr *actOnPrefixExpr(Expr *E, const OperatorInfo &Op);
   void checkFunctionParameterTypes(const ParameterList &Params, const ExprList &Exprs);
   Expr *actOnFunctionCallExpr(Identifier &FunId, ExprList &ParamExprs);
   Expr *actOnVariableExpr(Identifier &Id);
};

} // namespace llox

#endif