#ifndef LLOX_SEMA_TYPECHECKER_H
#define LLOX_SEMA_TYPECHECKER_H

#include "llox/Sema/Scope.h"

namespace llox {

class Sema;

class TypeChecker {
   Scope *GlobalScope;
   Sema *Sem;

public:
   TypeChecker(Scope *GScope, Sema *Sem);

   bool checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty);
   TypeDecl *getInfixTy(TypeDecl *Ty1, TypeDecl *Ty2);
   bool checkAssignmentTypes(TypeDecl *Ty1, TypeDecl *Ty2);
   bool checkFunctionRetTy(FunctionDecl *FunDecl, StmtList &FunStmts);
   void checkFunctionParameterTypes(const ParameterList &Params, const ExprList &Exprs);
};

} // namespace llox

#endif