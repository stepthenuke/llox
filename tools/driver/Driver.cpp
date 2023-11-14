#include "llox/AST/AST.h"
#include "llox/AST/ASTPrinter.h"
#include "llox/Basic/LLVM.h"
#include "llox/Parser/Parser.h"
#include "llox/Sema/Sema.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace llvm;
using namespace llox;

int main(int argc_, char **argv_) {

   // for (unsigned short i = 0; i < tok::TOKEN_AMT; ++i) {
   //    std::cout << tok::TokNames[i] << std::endl;
   // }

   llvm::InitLLVM X(argc_, argv_);

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

      StmtList Stmts;
      llvm::outs() << Par.parseStmtList(Stmts) << "\n";
      print(Stmts);

      // Expr *E = nullptr;
      // llvm::outs() << Par.parseExpr(E) << "\n";
      // print(E);

      // DeclList Decls;
      // while (!Par.parseDecl(Decls))
      //    llvm::outs() << 0 << " " << "\n";
      // printDeclList(Decls);
   }

   return 0;
}