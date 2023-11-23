#include "llox/CodeGen/CGCompilationUnit.h"
#include "llox/CodeGen/CGFunction.h"

namespace llox {

CGCompilationUnit::CGCompilationUnit(llvm::Module *M) 
   : M(M) 
{
   VoidTy = llvm::Type::getVoidTy(getLLVMContext());
   Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
   DoubleTy = llvm::Type::getDoubleTy(getLLVMContext());
}

llvm::GlobalObject *CGCompilationUnit::getGlobal(const Decl *D) {
   return Globals[D];
}

llvm::LLVMContext &CGCompilationUnit::getLLVMContext() {
   return M->getContext();
}

llvm::Module *CGCompilationUnit::getModule() {
   return M;
}

llvm::Type *CGCompilationUnit::convertType(const TypeDecl *Ty) {
   if (Ty->getName() == "double")
      return DoubleTy;
   else if (Ty->getName() == "bool")   
      return Int1Ty;
   return VoidTy;
}

void CGCompilationUnit::run(const CompilationUnitDecl *CU) {
   CompUnitD = CU;
   for (auto *Stm : CU->getStmts()) {
      if (auto *VarD = dyn_cast<VariableDecl>(Stm)) {
         llvm::GlobalVariable *V = new llvm::GlobalVariable(
            *M, 
            convertType(VarD->getType()), 
            false, 
            llvm::GlobalValue::PrivateLinkage, 
            nullptr,
            VarD->getName()
         );
         Globals[VarD] = V;
      }
      else if (auto *FunD = dyn_cast<FunctionDecl>(Stm)) {
         CGFunction CGFun(*this);
         llvm::Function *Fn = CGFun.run(FunD);
         Globals[FunD] = Fn;
      }
   }
}

const CompilationUnitDecl *CGCompilationUnit::getCompilationUnitDecl() {
   return CompUnitD;
}

} // namespace llox