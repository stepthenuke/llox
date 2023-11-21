#include <optional>

#include "llox/AST/AST.h"
#include "llox/AST/ASTPrinter.h"
#include "llox/Basic/LLVM.h"
#include "llox/CodeGen/CodeGenerator.h"
#include "llox/Parser/Parser.h"
#include "llox/Sema/Sema.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;
using namespace llox;

static codegen::RegisterCodeGenFlags CGF;

llvm::TargetMachine *createTargetMachine() {
   llvm::Triple Triple {llvm::Triple::normalize("aarch64-linux-gnu")};
   llvm::TargetOptions TargetOptions = codegen::InitTargetOptionsFromCodeGenFlags(Triple);
   std::string CPUStr  = codegen::getCPUStr();
   std::string FeaturesStr = codegen::getFeaturesStr();

   std::string Error;
   const llvm::Target *Target = llvm::TargetRegistry::lookupTarget(codegen::getMArch(), Triple, Error);
   if (!Target) {
      llvm::WithColor::error(llvm::errs()) << Error;
      return nullptr;
   }
   llvm::TargetMachine *TM = Target->createTargetMachine(
      Triple.getTriple(), 
      CPUStr, 
      FeaturesStr, 
      TargetOptions, 
      std::optional<llvm::Reloc::Model>(codegen::getRelocModel())
   );
   return TM;
}

int main(int argc_, char **argv_) {

   llvm::InitLLVM X(argc_, argv_);

   InitializeAllTargets();
   InitializeAllTargetMCs();
   InitializeAllAsmPrinters();
   InitializeAllAsmParsers();

   // for (unsigned short i = 0; i < tok::TOKEN_AMT; ++i) {
   //    std::cout << tok::TokNames[i] << std::endl;
   // }

   llvm::SmallVector<const char *, 256> argv(argv_ + 1, argv_ + argc_);

   llvm::outs() << "llox v.0.0228_alpha" << "\n";

   for (const char *F : argv) {
      llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> 
         FileOrErr = llvm::MemoryBuffer::getFile(F);
      if (std::error_code BufferError = FileOrErr.getError()) {
         llvm::errs() << "Error reading " << F << "; " 
            << BufferError.message() << "\n";
         continue;
      }
      
      SourceMgr SrcMgr;
      SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
      auto Lex = Lexer(SrcMgr);

      auto Sem = Sema();
      auto Par = Parser(Lex, Sem);
      
      // Token Tok;
      // Tok.setKind(tok::unknown);
      // while (Tok.isNot(tok::eof)) {
      //    Lex.getNextToken(Tok);
      // }

      // Expr *E = nullptr;
      // llvm::outs() << Par.parseExpr(E) << "\n";
      // print(E);

      // DeclList Decls;
      // while (!Par.parseDecl(Decls))
      //    llvm::outs() << 0 << " " << "\n";
      // printDeclList(Decls);


      CompilationUnitDecl *CompUnit = Par.parse();
      print(CompUnit);

      llvm::TargetMachine *TM = createTargetMachine();
      llvm::LLVMContext Context;
      if (CodeGenerator *CG = CodeGenerator::create(Context, TM)) {
         std::unique_ptr<llvm::Module> M = CG->run(CompUnit, F);
         delete CG;
      }
   }

   return 0;
}
