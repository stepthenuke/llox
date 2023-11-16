#include "llox/Sema/Sema.h"

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

// We can enter in scope of function or block
void Sema::enterScope(Stmt *D) {
   CurScope = new Scope(CurScope);
   CurDecl = D;
}

void Sema::leaveScope() {
   if (!CurScope)
      llvm_unreachable("can't leave non-existing scope");
   Scope *Parent = CurScope->getParent();
   delete CurScope;
   CurScope = Parent;
   if (auto *D = dyn_cast_or_null<Decl>(CurDecl)) {
      CurDecl = D->getEnclosingDecl();
   }
   else if (auto *B = dyn_cast_or_null<BlockStmt>(CurDecl)) {
      CurDecl = B->getEnclosingDecl();
   }
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

CompilationUnitDecl *Sema::actOnCompilationUnit(StringRef Name) {
   CompilationUnitDecl *CU = new CompilationUnitDecl(CurDecl, SMLoc(), Name);
   if (!CurScope->insert(CU))
      llvm_unreachable("Same name for comp unit");
   return CU;
}

void Sema::actOnCompilationUnit(CompilationUnitDecl *CompUnit, StmtList Stmts) {
   CompUnit->setStmts(Stmts);
}

void Sema::actOnIfStmt(StmtList &Stmts, Expr *Cond, StmtList &IfElseStmts) {
   IfStmt *Stm = nullptr;
   if (IfElseStmts.size() == 1) {
      if (auto *B1 = dyn_cast<BlockStmt>(IfElseStmts.front()))
         Stm = new IfStmt(Cond, *B1);
      else
         llvm_unreachable("incorrect stmt class for IfStmts");
   }
   else if (IfElseStmts.size() == 2) {
      auto *B1 = dyn_cast<BlockStmt>(IfElseStmts.front());
      auto *B2 = dyn_cast<BlockStmt>(IfElseStmts.back());
      if (B1 && B2) 
         Stm = new IfStmt(Cond, *B1, *B2);
      else
         llvm_unreachable("incorrect stmt class for IfStmts or ElseStmts"); 
   }
   else
      llvm_unreachable("incorrect number of stmts for IfStmt");

   Stmts.push_back(Stm);
}

void Sema::actOnWhileStmt(StmtList &Stmts, Expr *Cond, StmtList &WhileStmts) {
   if (WhileStmts.size() != 1)
      llvm_unreachable("incorrect number of stmts for WhileStmt");

   WhileStmt *Stm = nullptr;
   if (auto *B = dyn_cast<BlockStmt>(WhileStmts.front())) 
      Stm = new WhileStmt(Cond, *B);
   else 
      llvm_unreachable("incorrect stmt class for WhileStmts");  
   Stmts.push_back(Stm);
}

void Sema::actOnReturnStmt(StmtList &Stmts, Expr *E) {
   auto *Stm = new ReturnStmt(E);
   Stmts.push_back(Stm);
}

BlockStmt *Sema::actOnBlockStmt(StmtList &Stmts) {
   auto *Stm = new BlockStmt(CurDecl);
   Stmts.push_back(Stm);
   return Stm;
}

void Sema::actOnBlockStmt(BlockStmt *Block, StmtList &BlockStmts) {
   Block->setStmts(BlockStmts);
}

void Sema::actOnExprStmt(StmtList &Stmts, Expr *E) {
   ExprStmt *ES = new ExprStmt(E);
   Stmts.push_back(ES);
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

void Sema::actOnFunctionBlock(StmtList &Decls, FunctionDecl *FunDecl, StmtList &FunStmts) {
   FunDecl->setStmts(FunStmts);
   Decls.push_back(FunDecl);
}

void Sema::actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, StmtList &ParTypes) {
   if (!CurScope)
      llvm_unreachable("current scope isn't set");
   assert(ParIds.size() == ParTypes.size());

   auto TypeIdx = ParTypes.begin();
   for (auto Idx = ParIds.begin(), E = ParIds.end(); Idx != E; ++Idx, ++TypeIdx) {
      if (auto *Ty = dyn_cast<TypeDecl>(*TypeIdx)) {
         SMLoc Loc = Idx->first;
         StringRef Name = Idx->second;
         ParameterDecl *ParDecl = new ParameterDecl(CurDecl, Loc, Name, Ty, true);
         if (CurScope->insert(ParDecl)) {
            Params.push_back(ParDecl);
         }
         else
            llvm_unreachable("such parameter already exists");
      }
      else
         llvm_unreachable("not a type for parameter");
   }
}

Decl *Sema::actOnNameLookup(Stmt *Prev, SMLoc Loc, StringRef Name) {
   if (Decl *D = CurScope->lookup(Name))
      return D;
   return nullptr;
}

void Sema::actOnVariableDecl(StmtList &Decls, Identifier Id, Decl *D) {
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

void Sema::actOnAssignmentStmt(StmtList &Decls, Expr *E) {
   if (!CurScope)
      llvm_unreachable("no current scope");

   if (auto *V = dyn_cast<VariableDecl>(Decls.back())) {
      if (V->getType() != E->getType())
         llvm_unreachable("incompatible types for assignment");
      AssignmentStmt *AssigStmt = new AssignmentStmt(V, E);
      Decls.push_back(AssigStmt);
   }
   else 
      llvm_unreachable("no such variable"); 
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
   if (auto *F = dyn_cast_or_null<FunctionDecl>(D)) {
      checkFunctionParameterTypes(F->getParams(), ParamExprs);
      return new FunctionCallExpr(F, ParamExprs);
   } 
   llvm_unreachable("no such function for call");
   return nullptr;
}

Expr *Sema::actOnObjectExpr(Identifier &Id) {
   Decl *D = actOnNameLookup(CurDecl, Id.first, Id.second);
   if (auto *V = dyn_cast_or_null<VariableDecl>(D)) {
      return new ObjectExpr(V);
   }
   else if (auto *P = dyn_cast_or_null<ParameterDecl>(D)) {
      return new ObjectExpr(P);
   }
   llvm_unreachable("object (variable or parameter) is not declared");
   return nullptr;
}

} // namespace llox
