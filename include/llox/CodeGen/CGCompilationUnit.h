#ifndef LLOX_CODEGEN_CGCOMPILATIONUNIT_H
#define LLOX_CODEGEN_CGCOMPILATIONUNIT_H

#include "llox/AST/AST.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace llox {

class CGCompilationUnit {
   llvm::Module *M;
   const CompilationUnitDecl *CompUnitD;
   llvm::DenseMap<const Decl*, llvm::GlobalObject*> Globals;

public:
   llvm::Type *VoidTy;
   llvm::Type *Int1Ty;
   llvm::Type *Int32Ty;
   llvm::Type *DoubleTy;

public:
   CGCompilationUnit(llvm::Module *M);

   llvm::GlobalObject *getGlobal(const Decl *D);

   llvm::LLVMContext &getLLVMContext();
   llvm::Module *getModule();
   const CompilationUnitDecl *getCompilationUnitDecl();

   void run(const CompilationUnitDecl *CU);

   llvm::Type *convertType(const TypeDecl *Ty);
};

} // namespace llox

#endif