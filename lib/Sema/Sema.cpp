#include "llox/Sema/Sema.h"
#include "llox/Sema/Scope.h"

namespace llox {

bool Sema::checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty) {
   switch (OpKind) {
   case tok::equal:
   case tok::bang_equal:
   case tok::equal_equal:
      return Ty == BoolType || Ty == DoubleType;
   case tok::greater:
   case tok::greater_equal:
   case tok::less:
   case tok::less_equal:
   case tok::plus:
   case tok::minus:
   case tok::star:
   case tok::slash:
      return Ty == DoubleType;
   case tok::bang:
   case tok::kw_and:
   case tok::kw_or:
      return Ty == BoolType;
   default:
      llvm::outs() <<tok::getTokenName(OpKind) << " ";
      llvm_unreachable("unknown operator");
   }
   return false;
}

void Sema::enterScope(Decl *D) {
   CurScope = new Scope(CurScope);
   CurDecl = D;
}

void Sema::leaveScope() {
   if (!CurScope)
      llvm_unreachable("can't leave non-existing scope");
   Scope *Parent = CurScope->getParent();
   delete CurScope;
   CurScope = Parent;
   CurDecl = CurDecl->getEnclosingDecl();
}

Sema::Sema()
   : CurScope(new Scope()), CurDecl(nullptr) {
   DoubleType = new GlobalTypeDecl(CurDecl, SMLoc(), "double");
   BoolType = new GlobalTypeDecl(CurDecl, SMLoc(), "bool");
   TrueLiteral = new BoolLiteral(true, BoolType);
   FalseLiteral = new BoolLiteral(false, BoolType);
   CurScope->insert(DoubleType);
   CurScope->insert(BoolType);
}

FunctionDecl *Sema::actOnFunctionDecl(SMLoc Loc, StringRef Name) {
   FunctionDecl *FD = new FunctionDecl(CurDecl, Loc, Name);
   if (!CurScope->insert(FD))
      llvm_unreachable("Redeclaration of function");
   return FD;
}

void Sema::actOnFunctionParamList(FunctionDecl *FunDecl, ParameterList &Params, Decl *RetTy) {
   FunDecl->setParams(Params);
   auto CastedRetTy = llvm::dyn_cast_or_null<TypeDecl>(RetTy);
   if (!CastedRetTy && RetTy)
      llvm_unreachable("bad return type");
   else
      FunDecl->setRetType(CastedRetTy);
}

void Sema::actOnFunctionBlock(DeclList &Decls, FunctionDecl *FunDecl, DeclList &FunDecls, StmtList &FunStmts) {
   FunDecl->setDecls(FunDecls);
   FunDecl->setStmts(FunStmts);
   Decls.push_back(FunDecl);
}

void Sema::actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, DeclList &ParTypes) {
   if (!CurScope)
      llvm_unreachable("current scope isn't set");
   assert(ParIds.size() == ParTypes.size());

   auto TypeIdx = ParTypes.begin();
   for (auto Idx = ParIds.begin(), E = ParIds.end(); Idx != E; ++Idx, ++TypeIdx) {
      if (auto *Ty = dyn_cast<TypeDecl>(*TypeIdx)) {
         SMLoc Loc = Idx->first;
         StringRef Name = Idx->second;
         ParameterDecl *ParDecl = new ParameterDecl(CurDecl, Loc, Name, Ty, true);
         if (CurScope->insert(ParDecl))
            Params.push_back(ParDecl);
         else
            llvm_unreachable("such parameter already exists");
      }
      else
         llvm_unreachable("not a type for parameter");
   }

}

Decl *Sema::actOnNameLookup(Decl *Prev, SMLoc Loc, StringRef Name) {
   if (!Prev) {
      if (Decl *D = CurScope->lookup(Name))
         return D;
   }
   return nullptr;
}

void Sema::actOnVariableDecl(DeclList &Decls, Identifier Id, Decl *D) {
   if (!CurScope)
      llvm_unreachable("no current scope");
   
   if (TypeDecl *Ty = dyn_cast<TypeDecl>(D)) {
      SMLoc Loc = Id.first;
      StringRef Name = Id.second;
      VariableDecl *Decl = new VariableDecl(CurDecl, Loc, Name, Ty);
      if (CurScope->insert(Decl))
         Decls.push_back(Decl);
      else
         llvm_unreachable("variable already defined");
   }
   else 
      llvm_unreachable("no such type in var decl");
}

Expr *Sema::actOnDoubleLiteral(SMLoc Loc, StringRef Literal) {
   return new DoubleLiteral(Loc, llvm::APFloat(llvm::APFloat::IEEEdouble(), Literal), DoubleType);
}

Expr *Sema::actOnBoolLiteral(tok::TokenKind K) {
   if (K == tok::kw_true)
      return TrueLiteral;
   else
      return FalseLiteral;
}

Expr *Sema::actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op) {
   if (!Left)
      return Right;
   if (!Right)
      return Left;

   if (Left->getType() != Right->getType())
      llvm_unreachable("incompatible types");

   TypeDecl *Ty = Left->getType();
   if (!checkOperatorType(Op.getKind(), Ty))
      llvm_unreachable("incompatible operator for vals");

   bool RetTypeIsBool = op::isLogicalBinOp(Op.getKind());
   Ty = RetTypeIsBool ? BoolType : Ty;
   return new InfixExpr(Left, Right, Op, Ty);
}

Expr *Sema::actOnPrefixExpr(Expr *E, const OperatorInfo &Op) {
   if (!E)
      return nullptr;

   TypeDecl *Ty = E->getType();
   if (!checkOperatorType(Op.getKind(), Ty))
      llvm_unreachable("incompatible operator for val");

   return new PrefixExpr(E, Op, Ty);
}

void Sema::checkFunctionParameterTypes(const ParameterList &Params, const ExprList &Exprs) {
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

Expr *Sema::actOnFunctionCallExpr(Identifier &FunId, ExprList &ParamExprs) {
   Decl *D = actOnNameLookup(CurDecl, FunId.first, FunId.second);
   D = new FunctionDecl(nullptr, SMLoc(), StringRef("TEST_FUNC"));
   if (auto *F = dyn_cast<FunctionDecl>(D)) {
      // checkFunctionParameterTypes(F->getParams(), ParamExprs);
      F->setRetType(DoubleType); 
      return new FunctionCallExpr(F, ParamExprs);
   }
   llvm_unreachable("no call function");
   return nullptr;
}

Expr *Sema::actOnVariableExpr(Identifier &Id) {
   Decl *D = actOnNameLookup(CurDecl, Id.first, Id.second);
   // what're we doing here?
   return new DoubleLiteral(Id.first,  llvm::APFloat(llvm::APFloat::IEEEdouble(), 228), DoubleType);
   return nullptr;
}

} // namespace llox
