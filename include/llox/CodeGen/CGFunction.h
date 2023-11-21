#ifndef LLOX_CODEGEN_CGFUNCTION_H
#define LLOX_CODEGEN_CGFUNCTION_H

#include "llox/AST/AST.h"
#include "llox/CodeGen/CGCompilationUnit.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"

namespace llox {

class CGFunction {
   CGCompilationUnit &CGCUnit;
   const FunctionDecl *FunD;

   llvm::FunctionType *FnTy;
   llvm::Function *Fn;

   llvm::IRBuilder<> Builder;
   llvm::BasicBlock *CurBlock;

   llvm::DenseMap<const Decl*, llvm::Value*> Defs;

private:
   void setCurBlock(llvm::BasicBlock *BB);

   void writeVariable(const Decl *D, llvm::Value *V);
   llvm::Value *readVariable(const Decl *D);

   llvm::AllocaInst *createEntryBlockAlloca(const Decl *D);
   void writeLocalVariable(const Decl *D, llvm::Value *V);
   llvm::Value *readLocalVariable(const Decl *D);

   llvm::Type *getLLVMType(const Stmt *Stm);
   llvm::FunctionType *createFunctionType(const FunctionDecl *FunD);
   llvm::Function *createFunction(const FunctionDecl *FunD, llvm::FunctionType *FTy);

private:
   void emit(const StmtList &Stmts);
   void emit(const IfStmt *Stm);
   void emit(const WhileStmt* Stm);
   void emit(const ReturnStmt *Stm);
   void emit(const BlockStmt *Stm);
   void emit(const ExprStmt *Stm);

   llvm::Value *emit(const Expr *Exp);
   llvm::Value *emit(const InfixExpr *Exp);
   llvm::Value *emit(const PrefixExpr *Exp);
   llvm::Value *emit(const FunctionCallExpr *Exp);
   llvm::Value *emit(const ObjectExpr *Exp);
   llvm::Value *emit(const AssignmentExpr *Exp);

public:
   CGFunction(CGCompilationUnit &CGCUnit);
   void run(const FunctionDecl *FunD);
};

} // namespace llox

#endif