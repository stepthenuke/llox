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
static void print(const SelectorList &SL);
static void print(const Stmt *S);

static void print(const Expr *E) {
   if (!E) return;

   printSpaces();
   if (auto *Exp = dyn_cast<IntLiteral>(E)) {
      llvm::SmallVector<char, 16> Buffer;
      Exp->getValue().toString(Buffer);
      llvm::outs() << "IntLiteral " << Buffer << "\n";
   }
   else if (auto *Exp = dyn_cast<DoubleLiteral>(E)) {
      llvm::SmallVector<char, 16> Buffer;
      Exp->getValue().toString(Buffer);
      llvm::outs() << "DoubleLiteral " << Buffer << "\n";
   }
   else if (auto *Exp = dyn_cast<BoolLiteral>(E)) {
      llvm::outs() << "BoolLiteral " << (Exp->getValue() ? "true" : "false") << "\n";
   }
   else if (auto *Exp = dyn_cast<StringLiteral>(E)) {
      llvm::outs() << "StringLiteral \"" << Exp->getData() << "\"\n";
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
          << (Func->getRetType() ? Func->getRetType()->getName() : "nil") << "> \n";
      increaseIndentation();
      print(Exp->getParams());
      decreaseIndentation();
   }
   else if (auto *Exp = dyn_cast<ObjectExpr>(E)) {
      llvm::outs() << "ObjectExpr " << " <";
      if (auto *Ty = dyn_cast<ArrayTypeDecl>(E->getType()))
         llvm::outs() << "array ";
      llvm::outs() << Exp->getType()->getName() << ">";
      print(Exp->getObjectDecl());
      if (Exp->getSelectors().size() <= 0) 
         return;
      printLine();
      increaseIndentation();
      print(Exp->getSelectors());
      decreaseIndentation();
      printLine();
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
      llvm::outs() << "CompUnit " << Stm->getStmts().size() << "\n";
      printLine(); 
      print(Stm->getStmts());
      printLine();
   }
   else if (auto *Stm = dyn_cast<ExprStmt>(S)) {
      llvm::outs() << "ExprStmt ";
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
      llvm::outs() << "VariableDecl " << Stm->getName() << " <";
      if (auto *Ty = dyn_cast<ArrayTypeDecl>(Stm->getType()))
         llvm::outs() << "array ";
      llvm::outs() << Stm->getType()->getName() << "> " << Stm << "\n";
   }
   else if (auto *Stm = dyn_cast<ParameterDecl>(S)) {
      llvm::outs() << "ParameterDecl " << Stm->getName() << " isVar:" << Stm->isVar() << " <";
      if (isa<ArrayTypeDecl>(Stm->getType()))
         llvm::outs() << "array ";
      llvm::outs() << Stm->getType()->getName() << "> " << Stm << "\n"; 
   }
   else if (auto *Stm = dyn_cast<FunctionDecl>(S)) {
      llvm::outs() << "FunctionDecl " << Stm->getName() << " <" 
         << (Stm->getRetType() ? Stm->getRetType()->getName() : "nil") << "> \n";
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
      llvm::outs() << "GlobalTypeDecl " << Stm->getName() << "\n";
   }
   else if (auto *Stm = dyn_cast<StructTypeDecl>(S)) {
      llvm::outs() << "ClassTypeDecl " << Stm->getName() << "\n";
      printLine();
      increaseIndentation();
      auto Fields = Stm->getFields();
      for (auto &F : Fields) {
         print(F.second.second);
      }
      decreaseIndentation();
      printLine();
   }
   else if (auto *Stm = dyn_cast<Field>(S)) {
      llvm::outs() << "Field " << Stm->getName() << " <" ;
      if (isa<ArrayTypeDecl>(Stm->getType()))
         llvm::outs() << "array ";
      llvm::outs() << Stm->getType()->getName() << "> " << Stm << "\n";
   }
}

static void print(const Selector *S) {
   if (!S)
      return;

   printSpaces();
   
   if (auto *Sel = dyn_cast<FieldSelector>(S)) {
      llvm::outs() << "Field " << Sel->getName() << " [" << Sel->getIndex() << "] " << " <" << Sel->getType()->getName() << ">\n";
   }
   else if (auto *Sel = dyn_cast<IndexSelector>(S)) {
      llvm::outs() << "Index ";
      print(Sel->getIndex());
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

static void print(const SelectorList &SL) {
   for (auto S : SL)
      print(S);
}

} // namespace llox

#endif