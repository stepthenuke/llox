#include "llox/CodeGen/CodeGenerator.h"

namespace llox {
   
CodeGenerator *CodeGenerator::create(llvm::LLVMContext &Context, llvm::TargetMachine *TM) {
   return new CodeGenerator(Context, TM);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(CompilationUnitDecl *CU, std::string FileName) {
   std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>(FileName, Context);
   M->setTargetTriple(TM->getTargetTriple().getTriple());
   M->setDataLayout(TM->createDataLayout());
   CGCompilationUnit CGCompUnit(M.get());
   CGCompUnit.run(CU);
   // M->print(llvm::errs(), nullptr);
   return M;
}

} // namespace llox