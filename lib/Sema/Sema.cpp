#include "llox/Sema/Sema.h"

namespace llox {

TypeDecl *Sema::getBool() {
   return BoolType;
}

TypeDecl *Sema::getInt() {
   return IntType;
}

TypeDecl *Sema::getDouble() {
   return DoubleType;
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
   : GlobalScope(new Scope()), CurScope(GlobalScope), CurDecl(nullptr), TyChecker(CurScope, this) {
   IntType = new GlobalTypeDecl(CurDecl, SMLoc(), "int");
   DoubleType = new GlobalTypeDecl(CurDecl, SMLoc(), "double");
   BoolType = new GlobalTypeDecl(CurDecl, SMLoc(), "bool");
   StringType = new GlobalTypeDecl(CurDecl, SMLoc(), "string");
   TrueLiteral = new BoolLiteral(true, BoolType);
   FalseLiteral = new BoolLiteral(false, BoolType);
   CurScope->insert(IntType);
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
   if (TyChecker.checkFunctionRetTy(FunDecl, FunStmts))
      llvm_unreachable("return of incorrect type");
   FunDecl->setStmts(FunStmts);
   Decls.push_back(FunDecl);
}

void Sema::actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, StmtList &ParTypes, llvm::SmallVector<bool, 8> &VarFlags) {
   if (!CurScope)
      llvm_unreachable("current scope isn't set");
   assert(ParIds.size() == ParTypes.size());

   auto TypeIdx = ParTypes.begin();
   auto VarIdx = VarFlags.begin();
   for (auto Idx = ParIds.begin(), E = ParIds.end(); Idx != E; ++Idx, ++TypeIdx, ++VarIdx) {
      if (auto *Ty = dyn_cast<TypeDecl>(*TypeIdx)) {
         SMLoc Loc = Idx->first;
         StringRef Name = Idx->second;
         ParameterDecl *ParDecl = new ParameterDecl(CurDecl, Loc, Name, Ty, *VarIdx);
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

bool Sema::actOnVariableDecl(StmtList &Decls, Identifier Id, Decl *D) {
   if (!CurScope)
      llvm_unreachable("no current scope");


   if (TypeDecl *Ty = dyn_cast<TypeDecl>(D)) {
      SMLoc Loc = Id.first;
      StringRef Name = Id.second; 

      if (auto *CD = cast_or_null<Decl>(CurDecl)) {
         if (isa<StructTypeDecl>(CD)) {
             Field *F = new Field(Loc, Name, Ty);
            Decls.push_back(F);
            return true;
         }
      }

      VariableDecl *Decl = new VariableDecl(CurDecl, Loc, Name, Ty);
      if (CurScope->insert(Decl))
         Decls.push_back(Decl);
      else
         llvm_unreachable("variable already defined");
   }
   else 
      llvm_unreachable("no such type in var decl");
   return false;
}

Expr *Sema::actOnIntLiteral(SMLoc Loc, StringRef Literal) {
   llvm::APInt Value(64, Literal, 10);
   return new IntLiteral(Loc, llvm::APSInt(Value, false), IntType);
}

Expr *Sema::actOnDoubleLiteral(SMLoc Loc, StringRef Literal) {
   return new DoubleLiteral(Loc, llvm::APFloat(llvm::APFloat::IEEEdouble(), Literal), DoubleType);
}

Expr *Sema::actOnStringLiteral(SMLoc Loc, StringRef Literal) {
   StringRef StringData = Literal.drop_back().drop_front();
   return new StringLiteral(Loc, StringData, StringType);
}

Expr *Sema::actOnBoolLiteral(tok::TokenKind K) {
   if (K == tok::kw_true)
      return TrueLiteral;
   else
      return FalseLiteral;
}

Expr *Sema::actOnAssignmentExpr(Expr *Left, Expr *Right) {
   if (auto *O = dyn_cast<ObjectExpr>(Left)) {
      return new AssignmentExpr(O, Right);
   }
   else
      llvm_unreachable("lhs of = operator is not assignable");
}

void *Sema::actOnAssignmentInit(StmtList &Decls, Expr *E) {
   if (auto *V = dyn_cast<VariableDecl>(Decls.back())) {
      if (TyChecker.checkAssignmentTypes(V->getType(), E->getType()))
         llvm_unreachable("incompatible types for assignment");
      AssignmentExpr *AssignExpr = new AssignmentExpr(V, E);
      actOnExprStmt(Decls, AssignExpr);
   }
   else 
      llvm_unreachable("no such variable"); 
}

Expr *Sema::actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op) {
   if (!Left)
      return Right;
   if (!Right)
      return Left;

   TypeDecl *Ty = TyChecker.getInfixTy(Left->getType(), Right->getType());
   if (!Ty)
      llvm_unreachable("incompatible types");

   if (Op.getKind() == tok::equal)
      return actOnAssignmentExpr(Left, Right);

   return new InfixExpr(Left, Right, Op, Ty);
}

Expr *Sema::actOnPrefixExpr(Expr *E, const OperatorInfo &Op) {
   if (!E)
      return nullptr;

   TypeDecl *Ty = E->getType();
   if (!TyChecker.checkOperatorType(Op.getKind(), Ty))
      llvm_unreachable("incompatible operator for val");

   return new PrefixExpr(E, Op, Ty);
}

Expr *Sema::actOnFunctionCallExpr(Identifier &FunId, ExprList &ParamExprs) {
   Decl *D = actOnNameLookup(CurDecl, FunId.first, FunId.second);
   if (auto *F = dyn_cast_or_null<FunctionDecl>(D)) {
      TyChecker.checkFunctionParameterTypes(F->getParams(), ParamExprs);
      return new FunctionCallExpr(F, ParamExprs);
   } 
   llvm_unreachable("no such function for call");
   return nullptr;
}

void Sema::actOnSelectorList(Expr *O, SelectorList &SL) {
   if (auto *ObjE = dyn_cast<ObjectExpr>(O)) {
      ObjE->setType(SL.back()->getType());
      ObjE->setSelectors(SL);
   }
   else
      llvm_unreachable("selector can't be used on global type");
}

TypeDecl *Sema::actOnFieldSelector(Stmt *O, SelectorList &SelList, StringRef Name) {
   if (auto *StructTy = dyn_cast<StructTypeDecl>(O)) {
      int Idx = StructTy->getFieldIndex(Name);
      if (Idx < 0)
         llvm_unreachable("no such field in struct");
      TypeDecl *FieldTy = StructTy->getFieldType(Name);
      SelList.push_back(new FieldSelector(Idx, Name, FieldTy));

      print(FieldTy);
      if (isa<StructTypeDecl>(FieldTy) || isa<ArrayTypeDecl>(FieldTy))
         return FieldTy;
   }
   else 
      llvm_unreachable("field selector can't be used on non-struct");
   return nullptr;
}

TypeDecl *Sema::actOnIndexSelector(Stmt *O, SelectorList &SelList, Expr *IdxE) {
   if (auto *ArrTy = dyn_cast<ArrayTypeDecl>(O)) {
      SelList.push_back(new IndexSelector(IdxE, ArrTy->getType()));
      auto *BaseTy = ArrTy->getType();
      if (isa<StructTypeDecl>(BaseTy) || isa<ArrayTypeDecl>(BaseTy))
         return BaseTy;
   }
   else
      llvm_unreachable("index selector can't be used on non-array");
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

StructTypeDecl *Sema::actOnStructDecl(StmtList &Decls, Identifier &Id) {
   StructTypeDecl *StructD = new StructTypeDecl(CurDecl, Id.first, Id.second);
   if (!CurScope->insert(StructD))
      llvm_unreachable("Redeclaration of class");
   Decls.push_back(StructD);
   return StructD;
}

void Sema::actOnStructFields(StructTypeDecl *StructD, StmtList &FieldStmts) {
   for (auto *Stm : FieldStmts) {
      if (auto *F = dyn_cast<Field>(Stm)) {
         if (!StructD->insertField(F)) {
            llvm::errs() << F->getName() << " ";
            llvm_unreachable("field already exists");
         }
      }
      else
         llvm_unreachable("not a field inside struct");
   }
}

TypeDecl *Sema::actOnArrayTypeDecl(Decl *Ty, Expr *Num) { 
   auto *I = cast<IntLiteral>(Num);
   if (I->getValue() <= llvm::APSInt::get(0))
      llvm_unreachable("array can't have length <=0");

   auto *BaseTy = cast<TypeDecl>(Ty);
   return new ArrayTypeDecl(nullptr, BaseTy->getLocation(), BaseTy->getName(), Num, BaseTy);
}

} // namespace llox
