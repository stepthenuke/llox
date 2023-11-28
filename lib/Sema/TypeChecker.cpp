#include "llox/Sema/TypeChecker.h"
#include "llox/Sema/Sema.h"

namespace llox {

TypeChecker::TypeChecker(Scope *GScope, Sema *Sem) 
   : GlobalScope(GScope), Sem(Sem) {}

bool TypeChecker::checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty) {
   switch (OpKind) {
   case tok::equal:
   case tok::bang_equal:
   case tok::equal_equal:
   case tok::bang:
   case tok::kw_and:
   case tok::kw_or:
      return Ty == Sem->getBool() || Ty == Sem->getInt() || Ty == Sem->getDouble();
   case tok::greater:
   case tok::greater_equal:
   case tok::less:
   case tok::less_equal:
   case tok::plus:
   case tok::minus:
   case tok::star:
   case tok::slash:
      return Ty == Sem->getDouble() || Ty == Sem->getInt();
   default:
      break;
   }

   return false;
}

bool TypeChecker::checkAssignmentTypes(TypeDecl *Ty1, TypeDecl *Ty2) {
   if (Ty1 == Ty2)
      return false;
   
   if (Ty1 == Sem->getDouble() && Ty2 == Sem->getInt())
      return false;

   return true;
}

TypeDecl *TypeChecker::getInfixTy(TypeDecl *Ty1, TypeDecl *Ty2) {
   if (Ty1 == Sem->getBool() || Ty2 == Sem->getBool())
      return Sem->getBool();
   
   if ((Ty1 == Sem->getInt() && Ty2 == Sem->getDouble()) || (Ty1 == Sem->getDouble() && Ty2 == Sem->getInt()))
      return Sem->getDouble();

   if (Ty1 == Sem->getDouble() && Ty1 == Ty2)
      return Ty1;

   if (Ty1 == Sem->getInt() && Ty1 == Ty2)
      return Ty1;

   return nullptr;
}

void TypeChecker::checkFunctionParameterTypes(const ParameterList &Params, const ExprList &Exprs) {
   if (Params.size() != Exprs.size())
      llvm_unreachable("params size != parameter exprs size");

   auto E = Exprs.begin();
   for (auto P = Params.begin(), PE = Params.end(); P != PE; ++P, ++E) {
      ParameterDecl *Par = *P;
      Expr *Expr = *E;
      if (Par->getType() != Expr->getType())
         llvm_unreachable("param and args types of function do not correspond");
   }
}

bool TypeChecker::checkFunctionRetTy(FunctionDecl *FunDecl, StmtList &FunStmts) {
   auto FunRetTy = llvm::dyn_cast_or_null<TypeDecl>(FunDecl->getRetType());

   for (auto *S : FunStmts) {
      if (auto *RetS = dyn_cast<ReturnStmt>(S)) {
         auto *RetTy = RetS->getRetType();
         if (FunRetTy == Sem->getDouble() && RetTy == Sem->getInt())
            continue;
         if ((RetTy != FunRetTy))
            return true;
      }
   }
   return false;
}

} // namespace llox
