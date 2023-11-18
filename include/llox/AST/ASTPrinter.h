#ifndef LLOX_AST_ASTPRINTER_H
#define LLOX_AST_ASTPRINTER_H 

#include <string>

#include "llox/AST/AST.h"
#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llox {

namespace {

int SpacesAmt = 0;

inline void printLine() {
   llvm::outs() << std::string(SpacesAmt, ' ') << std::string(30, '-') << "\n"; 
}

inline void printSpaces() {
   llvm::outs() << std::string(SpacesAmt, ' ');
}

inline void increaseIndentation() {
   SpacesAmt += 3;
}

inline void decreaseIndentation() {
   SpacesAmt -= 3;
}

} // namespace

static void print(const ParameterList &PL);
static void print(const DeclList &DL);
static void print(const StmtList &SL);
static void print(const ExprList &EL);
static void print(const Stmt *S);

static void print(const Expr *E) {
   if (!E) return;

   printSpaces();
   if (auto *Exp = dyn_cast<DoubleLiteral>(E)) {
      llvm::SmallVector<char, 16> Buffer;
      Exp->getValue().toString(Buffer);
      llvm::outs() << "DoubleLiteral " << Buffer << "\n";
   }
   else if (auto *Exp = dyn_cast<BoolLiteral>(E)) {
      llvm::outs() << "BoolLiteral " << (Exp->getValue() ? "true" : "false") << "\n";
   }
   else if (auto *Exp = dyn_cast<InfixExpr>(E)) {
      llvm::outs() << "InfixExpr <" << Exp->getType()->getName() << "> " 
         << tok::getTokenName(Exp->getOperatorInfo().getKind()) << ":\n";
      increaseIndentation();
      print(Exp->getLeft());
      print(Exp->getRight());
      decreaseIndentation();
   }
   else if (auto *Exp = dyn_cast<PrefixExpr>(E)) {
      llvm::outs() << "PrefixExpr <" << Exp->getType()->getName() << "> "  
         << tok::getTokenName(Exp->getOperatorInfo().getKind()) << ":\n";
      increaseIndentation();
      print(Exp->getExpr());
      decreaseIndentation();
   }
   else if (auto *Exp = dyn_cast<FunctionCallExpr>(E)) {
      const FunctionDecl *Func = Exp->getFunctionDecl();
      llvm::outs() << "FunctionCallExpr " << Func->getName() << " <"
          << Func->getRetType()->getName() << "> \n";
      increaseIndentation();
      print(Exp->getParams());
      decreaseIndentation();
   }
   else if (auto *Exp = dyn_cast<ObjectExpr>(E)) {
      llvm::outs() << "ObjectExpr ";
      print(Exp->getObjectDecl());
   }
   else if (auto *Exp = dyn_cast<AssignmentExpr>(E)) {
      llvm::outs() << "AssignmentExpr \n";
      increaseIndentation();
      print(Exp->getObject());
      printLine();
      print(Exp->getExpr());
      decreaseIndentation();
   }
}

static void print(const Stmt *S) {
   if (!S) return;

   printSpaces();

   if (auto *Stm = dyn_cast<CompilationUnitDecl>(S)) {
      llvm::outs() << "CompUnit\n";
      printLine();
      print(Stm->getStmts());
      printLine();
   }
   else if (auto *Stm = dyn_cast<ExprStmt>(S)) {
      print(Stm->getExpr());
   }
   else if (auto *Stm = dyn_cast<BlockStmt>(S)) {
      llvm::outs() << "Block\n";
      increaseIndentation();
      print(Stm->getStmts());
      decreaseIndentation();
   }
   else if (auto *Stm = dyn_cast<IfStmt>(S)) {
      llvm::outs() << "IfStmt\n";
      increaseIndentation();
      print(Stm->getCond());
      decreaseIndentation();
      printLine();
      increaseIndentation();
      print(&Stm->getIfStmts());
      llvm::outs() << "\n";
      print(&Stm->getElseStmts());
      decreaseIndentation();
      llvm::outs() << "\n";
   }
   else if (auto *Stm = dyn_cast<WhileStmt>(S)) {
      llvm::outs() << "WhileStmt\n";
      increaseIndentation();
      print(Stm->getCond());
      decreaseIndentation();
      printLine();
      increaseIndentation();
      print(&Stm->getWhileStmts());
      decreaseIndentation();
      llvm::outs() << "\n";
   }
   else if (auto *Stm = dyn_cast<ReturnStmt>(S)) {
      llvm::outs() << "ReturnStmt\n";
      increaseIndentation();
      print(Stm->getRetVal());
      decreaseIndentation();
   }
   else if (auto *Stm = dyn_cast<VariableDecl>(S)) {
      llvm::outs() << "VariableDecl " << Stm->getName() << " <" 
         << Stm->getType()->getName() << "> \n";
   }
   else if (auto *Stm = dyn_cast<ParameterDecl>(S)) {
      llvm::outs() << "ParameterDecl " << Stm->getName() << " <" 
         << Stm->getType()->getName() << "> \n"; 
   }
   else if (auto *Stm = dyn_cast<FunctionDecl>(S)) {
      llvm::outs() << "FunctionDecl " << Stm->getName() << " <" 
         << Stm->getRetType()->getName() << "> \n";
      printLine();
      increaseIndentation();
      print(Stm->getParams());
      decreaseIndentation();
      printLine();
      increaseIndentation();
      print(Stm->getStmts());
      decreaseIndentation();
      llvm::outs() << "\n";
   }
   else if (auto *Stm = dyn_cast<GlobalTypeDecl>(S)) {
      llvm::outs() << "GlobalTypeDecl" << Stm->getName() << "\n";
   }

}

static void print(const ParameterList &PL) {
   for (auto P : PL) 
      print(P);
}

static void print(const DeclList &DL) {
   for (auto D : DL)
      print(D);
}

static void print(const StmtList &SL) {
   for (auto E : SL)
      print(E);
}

static void print(const ExprList &EL) {
   for (auto E : EL) 
      print(E);
}

} // namespace llox

#endif