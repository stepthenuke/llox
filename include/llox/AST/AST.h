#ifndef LLOX_AST_AST_H
#define LLOX_AST_AST_H

#include <vector>
#include <string>

#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llox {

class OperatorInfo {
   SMLoc Loc;
   uint32_t Kind : 8;
   uint32_t BinPrec : 8;
   uint32_t UnPrec : 8;
   uint32_t IsUnspecified : 1;

public:
   OperatorInfo()
      : Loc(), Kind(tok::unknown), BinPrec(op::Prec_None), UnPrec(op::Prec_None), IsUnspecified(true) {}

   OperatorInfo(SMLoc Loc, tok::TokenKind Kind, bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), BinPrec(op::getBinaryPrec(Kind)), UnPrec(op::getUnaryPrec(Kind)), IsUnspecified(IsUnspecified) {}
   
   SMLoc getLocation() const {
      return Loc;
   }

   tok::TokenKind getKind() const {
      return static_cast<tok::TokenKind>(Kind);
   }

   op::OperatorPrec getUnPrec() const {
      return static_cast<op::OperatorPrec>(UnPrec);
   }

   op::OperatorPrec getBinPrec() const {
      return static_cast<op::OperatorPrec>(BinPrec);
   }

   bool isUnspecified() const {
      return IsUnspecified;
   } 
};

class Decl;
class Stmt;
class Expr;
class TypeDecl;
class ParameterDecl;
using Identifier = std::pair<SMLoc, StringRef>;

using DeclList = std::vector<Decl*>;
using StmtList = std::vector<Stmt*>;
using ExprList = std::vector<Expr*>;
using ParameterList = std::vector<ParameterDecl*>;
using IdentList = std::vector<Identifier>;

class Stmt {
public:
   enum StmtKind {
      SK_Expr,
      SK_If,
      SK_While,
      SK_Return,
      SK_Block,
      DK_Var,
      DK_Func,
      DK_Param,
      DK_CompUnit,
      DK_GlobalType,
      DK_TypeEnd,
      DK_End
   };

private:
   const StmtKind Kind;

protected:
   Stmt(StmtKind Kind) 
      : Kind(Kind) {}

public:
   StmtKind getKind() const {
      return Kind;
   }
};

class BlockStmt : public Stmt {
   Stmt *EnclosingDecl;
   StmtList Stmts;

public:
   BlockStmt(Stmt *EnclosingDecl = nullptr)
      : Stmt(SK_Block), EnclosingDecl(EnclosingDecl) {}

   BlockStmt(Stmt *EnclosingDecl, const StmtList Stmts)
      : Stmt(SK_Block), EnclosingDecl(EnclosingDecl), Stmts(Stmts) {}

   void setStmts(StmtList &SL) {
      Stmts = SL;
   }

   const StmtList &getStmts() const {
      return Stmts;
   }

   Stmt *getEnclosingDecl() const {
      return EnclosingDecl;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_Block;
   }  
};

class IfStmt : public Stmt {
   Expr *Cond;
   BlockStmt IfStmts;
   BlockStmt ElseStmts;

public:
   IfStmt(Expr *Cond, BlockStmt &IfStmts)
      : Stmt(SK_If), Cond(Cond), IfStmts(IfStmts) {}
   
   IfStmt(Expr *Cond, BlockStmt &IfStmts, BlockStmt &ElseStmts)
      : Stmt(SK_If), Cond(Cond), IfStmts(IfStmts), ElseStmts(ElseStmts) {}

   Expr *getCond() const {
      return Cond;
   }

   const BlockStmt &getIfStmts() const {
      return IfStmts;
   }

   const BlockStmt &getElseStmts() const {
      return ElseStmts;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_If;
   }
};

class WhileStmt : public Stmt {
   Expr *Cond;
   BlockStmt Stmts;

public:
   WhileStmt(Expr *Cond, BlockStmt &Stmts)
      : Stmt(SK_While), Cond(Cond), Stmts(Stmts) {}

   Expr *getCond() const {
      return Cond;
   }

   const BlockStmt &getWhileStmts() const {
      return Stmts;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_While;
   }
};

class ReturnStmt : public Stmt {
   Expr *RetVal;

public:
   ReturnStmt(Expr *RetVal)
      : Stmt(SK_Return), RetVal(RetVal) {}
   
   Expr *getRetVal() const {
      return RetVal;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_Return;
   } 
};

class ExprStmt : public Stmt {
   Expr *E;

public:
   ExprStmt(Expr *E)
      : Stmt(SK_Expr), E(E) {}
   
   Expr *getExpr() const {
      return E;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_Expr;
   }   
};


class Decl : public Stmt {
protected:
   Stmt *EnclosingDecl;
   SMLoc Loc;
   StringRef Name;

public:
   Decl(StmtKind Kind, Stmt *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Stmt(Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}

   SMLoc getLocation() const {
      return Loc;
   }

   StringRef getName() const {
      return Name;
   }

   Stmt *getEnclosingDecl() const {
      return EnclosingDecl;
   }

   static bool classof(const Stmt *S) {
      return S->getKind() >= DK_Var &&
             S->getKind() <= DK_End;
   }
};

class CompilationUnitDecl : public Decl {
   StmtList Stmts;

public:
   CompilationUnitDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DK_CompUnit, EnclosingDecl, Loc, Name) {}

   CompilationUnitDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name, StmtList &Stmts)
      : Decl(DK_CompUnit, EnclosingDecl, Loc, Name), Stmts(Stmts) {}

   const StmtList &getStmts() const {
      return Stmts;
   }
   void setStmts(const StmtList &SL) {
      Stmts = SL;
   }

public:
   static bool classof(const Stmt* D) {
      return D->getKind() == DK_CompUnit;
   }
};

class VariableDecl : public Decl {
   TypeDecl *Ty;

public:
   VariableDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name, TypeDecl *Ty)
      : Decl(DK_Var, EnclosingDecl, Loc, Name), Ty(Ty) {}
   
   TypeDecl *getType() const {
      return Ty;
   }

public:
   static bool classof(const Stmt *D) {
      return D->getKind() == DK_Var;
   }
};

class ParameterDecl : public Decl {
   TypeDecl *Ty;
   bool IsVar;

public:
   ParameterDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name, TypeDecl *Ty, bool IsVar)
      : Decl(DK_Param, EnclosingDecl, Loc, Name), Ty(Ty), IsVar(IsVar) {}
   
   bool isVar() const {
      return IsVar;
   }

   TypeDecl *getType() const {
      return Ty;
   }

public:
   static bool classof(const Stmt *D) {
      return D->getKind() == DK_Param;
   }
};

class FunctionDecl : public Decl {
   ParameterList Params;
   TypeDecl *RetType;
   StmtList Stmts;

public:
   FunctionDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DK_Func, EnclosingDecl, Loc, Name) {}

   FunctionDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name, ParameterList &Params,
      TypeDecl *RetType, StmtList &Stmts) 
      : Decl(DK_Func, EnclosingDecl, Loc, Name),
        Params(Params), RetType(RetType),  Stmts(Stmts) {}
   
   const ParameterList &getParams() const {
      return Params;
   }
   void setParams(ParameterList &PL) {
      Params = PL;
   }

   TypeDecl *getRetType() const {
      return RetType;
   }
   void setRetType(TypeDecl *Ty) {
      RetType = Ty;
   }

   const StmtList &getStmts() const {
      return Stmts;
   }
   void setStmts(StmtList &SL) {
      Stmts = SL;
   }

public:
   static bool classof(const Stmt *D) {
      return D->getKind() == DK_Func;
   }
};

class TypeDecl : public Decl {
protected:
   TypeDecl(StmtKind Kind, Stmt *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(Kind, EnclosingDecl, Loc, Name) {}

public:
   static bool classof(const Stmt *D) {
      return D->getKind() >= DK_GlobalType &&
             D->getKind() <= DK_TypeEnd;
   }
};

class GlobalTypeDecl : public TypeDecl {
public:
   GlobalTypeDecl(Stmt *EnclosingDecl, SMLoc Loc, StringRef Name)
      : TypeDecl(DK_GlobalType, EnclosingDecl, Loc, Name) {}

public:
   static bool classof(const Stmt *D) {
      return D->getKind() == DK_GlobalType;
   }
};

class Expr {
public:
   enum ExprKind {
      EK_Double,
      EK_Bool,
      EK_Infix,
      EK_Prefix,
      EK_Func
   };

private:
   const ExprKind Kind;
   TypeDecl *Ty;

protected:
   Expr(ExprKind Kind, TypeDecl *Ty)
      : Kind(Kind), Ty(Ty) {}

public:
   ExprKind getKind() const {
      return Kind;
   }

   TypeDecl *getType() const {
      return Ty;
   }

   void setType(TypeDecl *T) {
      Ty = T;
   }
};

class DoubleLiteral : public Expr {
   SMLoc Loc;
   llvm::APFloat Value;

public:
   DoubleLiteral(SMLoc Loc, const llvm::APFloat &Value, TypeDecl *Ty)
      : Expr(EK_Double, Ty), Loc(Loc), Value(Value) {}
   
   llvm::APFloat &getValue() {
      return Value;
   }

   const llvm::APFloat &getValue() const {
      return Value;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Double;
   }
};

class BoolLiteral : public Expr {
   bool Value;

public:
   BoolLiteral(bool Value, TypeDecl *Ty)
      : Expr(EK_Bool, Ty), Value(Value) {}
   
   bool getValue() const {
      return Value;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Bool;
   }  
};

class InfixExpr : public Expr {
   Expr *Left;
   Expr *Right;
   const OperatorInfo Op;

public:
   InfixExpr(Expr *Left, Expr *Right, OperatorInfo Op, TypeDecl *Ty) 
      : Expr(EK_Infix, Ty), Left(Left), Right(Right), Op(Op) {}

   Expr *getLeft() const {
      return Left;
   }

   Expr *getRight() const {
      return Right;
   }

   const OperatorInfo &getOperatorInfo() const {
      return Op;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Infix;
   }
};

class PrefixExpr : public Expr {
   Expr *E;
   const OperatorInfo Op;

public:
   PrefixExpr(Expr *E, OperatorInfo Op, TypeDecl *Ty)
      : Expr(EK_Prefix, Ty), E(E), Op(Op) {}

   Expr *getExpr() const {
      return E;
   }

   const OperatorInfo &getOperatorInfo() const {
      return Op;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Prefix;
   }
};

class FunctionCallExpr : public Expr {
   FunctionDecl *Func;
   ExprList Params;

public:
   FunctionCallExpr(FunctionDecl *Func, ExprList Params)
      : Expr(EK_Func, Func->getRetType()), Func(Func), Params(Params) {}

   FunctionDecl *getFunctionDecl() const {
      return Func;
   }

   const ExprList &getParams() const {
      return Params;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Func;
   }
};

} //namespace llox

#endif