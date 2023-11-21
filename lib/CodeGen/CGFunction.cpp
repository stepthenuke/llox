#include "llox/CodeGen/CGFunction.h"

namespace llox {

void CGFunction::setCurBlock(llvm::BasicBlock *BB) {
   CurBlock = BB;
   Builder.SetInsertPoint(CurBlock);
}

llvm::Type *CGFunction::getLLVMType(const Stmt *Stm) {
   if (auto *VarD = dyn_cast<VariableDecl>(Stm)) {
      return CGCUnit.convertType(VarD->getType());
   }
   else if (auto *ParD = dyn_cast<ParameterDecl>(Stm)) {
      return CGCUnit.convertType(ParD->getType());
   }
   else if (auto *TypD = dyn_cast<TypeDecl>(Stm)) {
      return CGCUnit.convertType(TypD);
   }
   return nullptr;
}

llvm::FunctionType *CGFunction::createFunctionType(const FunctionDecl *FunD) {
   llvm::Type *ReturnTy = getLLVMType(FunD->getRetType());
   auto Params = FunD->getParams();
   llvm::SmallVector<llvm::Type*, 8> ParamTypes;
   for (auto P : Params) {
      ParamTypes.push_back(getLLVMType(P));
   }
   return llvm::FunctionType::get(ReturnTy, ParamTypes, false);
}

llvm::Function *CGFunction::createFunction(const FunctionDecl *FunD, llvm::FunctionType *FTy) {
   llvm::Function *Fn = llvm::Function::Create(
      FTy, 
      llvm::Function::ExternalLinkage, 
      FunD->getName(),
      CGCUnit.getModule()
   );
   auto Params = FunD->getParams();
   auto ParI = Params.begin();
   for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++ParI) {
      if (auto *P = dyn_cast<ParameterDecl>(*ParI)) {
         I->setName(P->getName());
      }
   }
   return Fn;
}

void CGFunction::writeVariable(const Decl *D, llvm::Value *Val) {
   if (auto *VarD = dyn_cast<VariableDecl>(D)) {
      if (VarD->getEnclosingDecl() == CGCUnit.getCompilationUnitDecl())
         Builder.CreateStore(Val, CGCUnit.getGlobal(D));
      else 
         writeLocalVariable(D, Val);
   }
   else if (auto *ParD = dyn_cast<ParameterDecl>(D)) {
      if (ParD->getEnclosingDecl() == CGCUnit.getCompilationUnitDecl())
         Builder.CreateStore(Val, CGCUnit.getGlobal(D));
      else 
         writeLocalVariable(D, Val);
   }
   else 
      llvm::report_fatal_error("unsupported declaration");
}

llvm::Value *CGFunction::readVariable(const Decl *D) {
   if (auto *VarD = dyn_cast<VariableDecl>(D)) {
      if (VarD->getEnclosingDecl() == CGCUnit.getCompilationUnitDecl())
         return Builder.CreateLoad(getLLVMType(D), CGCUnit.getGlobal(D));
      else 
         return readLocalVariable(D);
   }
   else if (auto *ParD = dyn_cast<ParameterDecl>(D)) {
      if (ParD->getEnclosingDecl() == CGCUnit.getCompilationUnitDecl())
         return Builder.CreateLoad(getLLVMType(D), CGCUnit.getGlobal(D));
      else 
         return readLocalVariable(D);
   }
   else 
      llvm::report_fatal_error("unsupported declaration");   
}

llvm::AllocaInst *CGFunction::createEntryBlockAlloca(const Decl *D) {
   llvm::IRBuilder<> TmpBuilder(&Fn->getEntryBlock(), Fn->getEntryBlock().begin());
   return TmpBuilder.CreateAlloca(getLLVMType(D), nullptr, D->getName());
}

void CGFunction::writeLocalVariable(const Decl *D, llvm::Value *Val) {
   auto Dec = Defs.find(D);
   if (Dec == Defs.end()) {
      auto *Alloca = createEntryBlockAlloca(D);
      Defs[D] = Alloca;
   }
   Builder.CreateStore(Defs[D], Val);
}

llvm::Value *CGFunction::readLocalVariable(const Decl *D) {
   auto Dec = Defs.find(D);
   if (Dec == Defs.end())
      llvm::report_fatal_error("no declaration");

   return Builder.CreateLoad(getLLVMType(D), Defs[D]);
}

void CGFunction::emit(const StmtList &Stmts) {
   for (auto &&Stm : Stmts) {
      if (auto *S = dyn_cast<IfStmt>(Stm))
         emit(S);
      else if (auto *S = dyn_cast<WhileStmt>(Stm))
         emit(S);
      else if (auto *S = dyn_cast<ReturnStmt>(Stm))
         emit(S);
      else if (auto *S = dyn_cast<ExprStmt>(Stm))
         emit(S);
   }
}

void CGFunction::emit(const IfStmt *Stm) {
   llvm::BasicBlock *IfBB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "if.body", Fn);
   bool ElseFlag = Stm->getElseStmts().getStmts().size() > 0;
   llvm::BasicBlock *ElseBB = ElseFlag ?
      llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "else.body", Fn) : nullptr;
   llvm::BasicBlock *ContIfBB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "cont.if", Fn);

   llvm::Value *Cond = emit(Stm->getCond());
   Builder.CreateCondBr(Cond, IfBB, ElseFlag ? ElseBB : ContIfBB);

   setCurBlock(IfBB);
   emit(&Stm->getIfStmts());
   if (!CurBlock->getTerminator())
      Builder.CreateBr(ContIfBB);

   if (ElseFlag) {
      setCurBlock(ElseBB);
      emit(&Stm->getElseStmts());
      if (!CurBlock->getTerminator())
         Builder.CreateBr(ContIfBB);
   }

   setCurBlock(ContIfBB);
}

void CGFunction::emit(const WhileStmt* Stm) {
   llvm::BasicBlock *WhileCondBB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "while.cond", Fn);
   llvm::BasicBlock *WhileBodyBB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "while.body", Fn);
   llvm::BasicBlock *ContWhileBB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "cont.while", Fn);

   Builder.CreateBr(WhileCondBB);
   setCurBlock(WhileCondBB);
   llvm::Value *Cond = emit(Stm->getCond());
   Builder.CreateCondBr(Cond, WhileBodyBB, ContWhileBB);

   setCurBlock(WhileBodyBB);
   emit(&Stm->getWhileStmts());
   Builder.CreateBr(WhileCondBB);

   setCurBlock(ContWhileBB);
}

void CGFunction::emit(const ReturnStmt *Stm) {
   if (Stm->getRetVal()) {
      llvm::Value *RetVal = emit(Stm->getRetVal());
      Builder.CreateRet(RetVal);
   }
   else
      Builder.CreateRetVoid();
}

void CGFunction::emit(const BlockStmt *Stm) {
   emit(Stm->getStmts());
}

void CGFunction::emit(const ExprStmt *Stm) {
   emit(Stm->getExpr());
}

llvm::Value *CGFunction::emit(const Expr *Exp) {
   if (!Exp)
      return nullptr;

   if (auto *E = dyn_cast<DoubleLiteral>(Exp))
      return llvm::ConstantFP::get(CGCUnit.DoubleTy, E->getValue());
   else if (auto *E = dyn_cast<BoolLiteral>(Exp))
      return llvm::ConstantInt::get(CGCUnit.Int1Ty, E->getValue());
   else if (auto *E = dyn_cast<InfixExpr>(Exp))
      return emit(E);
   else if (auto *E = dyn_cast<PrefixExpr>(Exp))
      return emit(E);
   else if (auto *E = dyn_cast<FunctionCallExpr>(Exp))
      return emit(E);
   else if (auto *E = dyn_cast<AssignmentExpr>(Exp))
      return emit(E);
   else if (auto *E = dyn_cast<ObjectExpr>(Exp)) 
      return emit(E);
   return nullptr;
}

llvm::Value *CGFunction::emit(const InfixExpr *Exp) {
   auto *Left = emit(Exp->getLeft());
   auto *Right = emit(Exp->getRight());
   llvm::Value *Result = nullptr;

   switch (Exp->getOperatorInfo().getKind()) {
   case tok::plus:
      Result = Builder.CreateFAdd(Left, Right);
      break;
   case tok::minus:
      Result = Builder.CreateFSub(Left, Right);
      break;
   case tok::star:
      Result = Builder.CreateFMul(Left, Right);
      break;
   case tok::slash:
      Result = Builder.CreateFDiv(Left, Right);
      break;
   case tok::equal_equal:
      Result = Builder.CreateFCmpOEQ(Left, Right);
      break;
   case tok::bang_equal:
      Result = Builder.CreateFCmpONE(Left, Right);
      break;
   case tok::less:
      Result = Builder.CreateFCmpOLT(Left, Right);
      break;
   case tok::less_equal:
      Result = Builder.CreateFCmpOLE(Left, Right);
      break;
   case tok::greater:
      Result = Builder.CreateFCmpOGT(Left, Right);
      break;
   case tok::greater_equal:
      Result = Builder.CreateFCmpOGE(Left, Right);
      break;
   case tok::kw_and:
      Result = Builder.CreateAnd(Left, Right);
      break;
   case tok::kw_or:
      Result = Builder.CreateOr(Left, Right);
      break;
   default:
      llvm_unreachable("wrong operator");
   }

   return Result;
}

llvm::Value *CGFunction::emit(const PrefixExpr *Exp) {
   llvm::Value *Result = emit(Exp->getExpr());
   switch (Exp->getOperatorInfo().getKind()) {
   case tok::minus:
      Result = Builder.CreateFNeg(Result);
      break;
   case tok::bang:
      Result = Builder.CreateNot(Result);
      break;
   default:
      llvm_unreachable("wrong operator");
   }
   return Result;
}

llvm::Value *CGFunction::emit(const FunctionCallExpr *Exp) {
   llvm_unreachable("not implemented");
}

llvm::Value *CGFunction::emit(const ObjectExpr *Exp) {
   return readVariable(Exp->getObjectDecl());
}

llvm::Value *CGFunction::emit(const AssignmentExpr *Exp) {
   llvm::Value *Val = emit(Exp->getExpr());
   writeVariable(Exp->getObject()->getObjectDecl(), Val);
   return Val;
}

CGFunction::CGFunction(CGCompilationUnit &CGCUnit)
   : CGCUnit(CGCUnit), Builder(CGCUnit.getLLVMContext()), CurBlock(nullptr) {}

void CGFunction::run(const FunctionDecl *FunD) {
   this->FunD = FunD;
   FnTy = createFunctionType(FunD);
   Fn = createFunction(FunD, FnTy);

   llvm::BasicBlock *BB = llvm::BasicBlock::Create(CGCUnit.getLLVMContext(), "entry", Fn);
   setCurBlock(BB);

   auto Params = FunD->getParams();
   auto ParI = Params.begin();
   for (auto &Arg : Fn->args()) {
      llvm::AllocaInst *Alloca = createEntryBlockAlloca(*ParI);
      Builder.CreateStore(&Arg, Alloca);
      Defs[*ParI] = Alloca;
      ++ParI;
   }

   auto FnStmts = FunD->getStmts();
   emit(FnStmts);
   if (!CurBlock->getTerminator()) {
      Builder.CreateRetVoid();
   }
}

} // namespace llox