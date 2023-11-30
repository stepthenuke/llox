#include <algorithm>

#include "llox/CodeGen/CGCompilationUnit.h"
#include "llox/CodeGen/CGFunction.h"

namespace llox {

CGCompilationUnit::CGCompilationUnit(llvm::Module *M) 
   : M(M) 
{
   VoidTy = llvm::Type::getVoidTy(getLLVMContext());
   Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
   Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
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

llvm::Type *CGCompilationUnit::convertType(const TypeDecl *Type) {
   if (auto *Ty = dyn_cast<GlobalTypeDecl>(Type)) {
      if (Ty->getName() == "double")
         return DoubleTy;
      else if (Ty->getName() == "int")
         return Int32Ty;
      else if (Ty->getName() == "bool")   
         return Int1Ty;
   }
   else if (auto *Ty = dyn_cast<ArrayTypeDecl>(Type)) {
      auto *ElementType = convertType(Ty->getType());
      CGFunction TempCG(*this);
      auto *Size = TempCG.emit(Ty->getNum());
      if (auto *S = dyn_cast<llvm::ConstantInt>(Size)) 
         return llvm::ArrayType::get(ElementType, S->getZExtValue());
   }
   else if (auto *Ty = llvm::StructType::getTypeByName(getLLVMContext(), Type->getName()))
      return Ty;
   return VoidTy;
}

void CGCompilationUnit::run(const CompilationUnitDecl *CU) {
   CompUnitD = CU;
   auto Stmts = CU->getStmts();
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
      else if (auto *TypD = dyn_cast<StructTypeDecl>(Stm)) {
         llvm::SmallVector<std::pair<int, Stmt*>, 8> StructFieldPairs;
         auto FieldsMap = TypD->getFields();
         for (const auto &FieldPair : FieldsMap) {
            StructFieldPairs.push_back(FieldPair.second);
         }
         std::sort(StructFieldPairs.begin(), StructFieldPairs.end());
         llvm::SmallVector<llvm::Type*, 8> StructFields;
         for (auto &&FieldPair : StructFieldPairs) {
            if (auto *F = dyn_cast<Field>(FieldPair.second)) {
               llvm::Type *Ty = convertType(F->getType());
               StructFields.push_back(Ty);
            }
         }
         llvm::StructType::create(getLLVMContext(), StructFields, TypD->getName());
      }
   }
}

const CompilationUnitDecl *CGCompilationUnit::getCompilationUnitDecl() {
   return CompUnitD;
}

} // namespace llox