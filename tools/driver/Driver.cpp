#include <optional>

#include "llox/AST/AST.h"
#include "llox/AST/ASTPrinter.h"
#include "llox/Basic/LLVM.h"
#include "llox/CodeGen/CodeGenerator.h"
#include "llox/Parser/Parser.h"
#include "llox/Sema/Sema.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "llvm/Support/CodeGen.h"

using namespace llvm;
using namespace llox;

static codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string> InputFiles(cl::Positional, cl::desc("<input-files>"));
static llvm::cl::opt<bool> EmitLLVM("emit-llvm", cl::desc("Emit IR code"), cl::init(false));
static llvm::cl::opt<bool> EmitAST("emit-ast", cl::desc("Emit AST"), cl::init(false));

static const char *Head = "Compiler for (not) llox by lox";

llvm::TargetMachine *createTargetMachine() {
   llvm::Triple Triple {llvm::Triple::normalize("arm64-apple-macosx14.0.0")};
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

std::string outputFilename(StringRef InputFilename) {
   CodeGenFileType FileType = codegen::getFileType();
   std::string OutputFilename;
   if (InputFilename == "-")
      OutputFilename = "-";
   else {
      if (InputFilename.ends_with(".lox"))
         OutputFilename = InputFilename.drop_back(4).str();
      else
         OutputFilename = InputFilename.str();
      switch (FileType) {
      case CGFT_AssemblyFile:
         OutputFilename.append(EmitLLVM ? ".ll" : ".s");
         break;
      case CGFT_ObjectFile:
         OutputFilename.append(".o");
         break;
      case CGFT_Null:
         OutputFilename.append(".null");
         break;
      }
   }
   return OutputFilename;
}

#define HANDLE_EXTENSION(Ext)                          \
  llvm::PassPluginLibraryInfo get##Ext##PluginInfo();
#include "llvm/Support/Extension.def"

bool emit(StringRef Argv, llvm::Module *M, llvm::TargetMachine *TM, StringRef InputFilename) {
   PassBuilder PB(TM);

#define HANDLE_EXTENSION(Ext)                          \
  get##Ext##PluginInfo().RegisterPassBuilderCallbacks(PB);
#include "llvm/Support/Extension.def"
   
   LoopAnalysisManager LAM;
   FunctionAnalysisManager FAM;
   CGSCCAnalysisManager CGAM;
   ModuleAnalysisManager MAM;

   PB.registerModuleAnalyses(MAM);
   PB.registerCGSCCAnalyses(CGAM);
   PB.registerFunctionAnalyses(FAM);
   PB.registerLoopAnalyses(LAM);
   PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

   ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(OptimizationLevel::O2);

   std::error_code EC;
   sys::fs::OpenFlags OpenFlags = sys::fs::OF_None;
   CodeGenFileType FileType = codegen::getFileType();
   if (FileType == CGFT_AssemblyFile) {
      OpenFlags |= sys::fs::OF_Text;
   }
   raw_fd_ostream OS {outputFilename(InputFilename), EC, OpenFlags};
   if (EC) {
      WithColor::error(errs(), Argv) << EC.message() << "\n";
      return false;
   }

   legacy::PassManager CodeGenPM;
   CodeGenPM.add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
   if (FileType == CGFT_AssemblyFile && EmitLLVM) {
      CodeGenPM.add(createPrintModulePass(OS));
   }
   else {
      if (TM->addPassesToEmitFile(CodeGenPM, OS, nullptr, FileType)) {
         WithColor::error() << "No support for file type\n";
         return false;
      }
   }

   MPM.run(*M, MAM);
   CodeGenPM.run(*M);
   OS.flush();
   return true;
}

int main(int argc_, char **argv_) {

   llvm::InitLLVM X(argc_, argv_);

   InitializeAllTargets();
   InitializeAllTargetMCs();
   InitializeAllAsmPrinters();
   InitializeAllAsmParsers();

   llvm::cl::ParseCommandLineOptions(argc_, argv_, Head); 

   llvm::outs() << "llox v.0.0228_alpha" << "\n";

   llvm::TargetMachine *TM = createTargetMachine();
   if (!TM)
      exit(EXIT_FAILURE);
   
   for (const auto &F : InputFiles) {
      llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(F);
      if (std::error_code BufferError = FileOrErr.getError()) {
         llvm::WithColor::error(errs(), argv_[0]) << "Error reading " << F 
         << ": " << BufferError.message() << "\n";
      }

      SourceMgr SrcMgr;
      SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
      
      auto Lex = Lexer(SrcMgr);
      auto Sem = Sema();
      auto Par = Parser(Lex, Sem);
      CompilationUnitDecl *CompUnit = Par.parse();
      if (EmitAST)
         print(CompUnit);

      if (CompUnit) {
         llvm::LLVMContext Context;
         if (CodeGenerator *CG = CodeGenerator::create(Context, TM)) {
            std::unique_ptr<llvm::Module> M = CG->run(CompUnit, F);
            if (!emit(argv_[0], M.get(), TM, F))
               llvm::WithColor::error(errs(), argv_[0]) << "Error emitting\n"; 
            delete CG;
         }
      }

   }

   return 0;
}
