#ifndef LLOX_CODEGEN_CODEGENERATOR_H
#define LLOX_CODEGEN_CODEGENERATOR_H

#include <string>
#include "llox/AST/AST.h"
#include "llox/CodeGen/CGCompilationUnit.h"
#include "llvm/Target/TargetMachine.h"

namespace llox {

class CodeGenerator {
   llvm::LLVMContext &Context;
   llvm::TargetMachine *TM;
   CompilationUnitDecl *CU;

protected:
   CodeGenerator(llvm::LLVMContext &Context, llvm::TargetMachine *TM)
      : Context(Context), TM(TM), CU(nullptr) {}

public:
   static CodeGenerator *create(llvm::LLVMContext &Context, llvm::TargetMachine *TM);
   std::unique_ptr<llvm::Module> run(CompilationUnitDecl *CU, std::string FileName);
};

} // namespace llox

#endif