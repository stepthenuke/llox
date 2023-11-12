#ifndef LLOX_AST_AST_H
#define LLOX_AST_AST_H

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

class Decl {
public:
   enum DeclKind {
      DK_Var,
      DK_Func,
      DK_Param,
      DK_GlobalType,
      DK_TypeEnd
   };

private:
   const DeclKind Kind;

protected:
   Decl *EnclosingDecl;
   SMLoc Loc;
   StringRef Name;

public:
   Decl(DeclKind Kind, Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Kind(Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}

   DeclKind getKind() const {
      return Kind;
   }

   SMLoc getLocation() const {
      return Loc;
   }

   StringRef getName() const {
      return Name;
   }

   Decl *getEnclosingDecl() const {
      return EnclosingDecl;
   }
};

class VariableDecl : public Decl {
   TypeDecl *Ty;

public:
   VariableDecl(Decl *EnclosingDecl, SMLoc Loc, StringRef Name, TypeDecl *Ty)
      : Decl(DK_Var, EnclosingDecl, Loc, Name), Ty(Ty) {}
   
   TypeDecl *getType() {
      return Ty;
   }

public:
   static bool classof(const Decl *D) {
      return D->getKind() == DK_Var;
   }
};

class ParameterDecl : public Decl {
   TypeDecl *Ty;
   bool IsVar;

public:
   ParameterDecl(Decl *EnclosingDecl, SMLoc Loc, StringRef Name, TypeDecl *Ty, bool IsVar)
      : Decl(DK_Param, EnclosingDecl, Loc, Name), Ty(Ty), IsVar(IsVar) {}
   
   bool isVar() const {
      return IsVar;
   }

   TypeDecl *getType() {
      return Ty;
   }

public:
   static bool classof(const Decl *D) {
      return D->getKind() == DK_Param;
   }
};

class FunctionDecl : public Decl {
   ParameterList Params;
   TypeDecl *RetType;
   DeclList Decls;
   StmtList Stmts;

public:
   FunctionDecl(Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DK_Func, EnclosingDecl, Loc, Name) {}

   FunctionDecl(Decl *EnclosingDecl, SMLoc Loc, StringRef Name, ParameterList &Params,
      TypeDecl *RetType, DeclList &Decls, StmtList &Stmts) 
      : Decl(DK_Func, EnclosingDecl, Loc, Name),
        Params(Params), RetType(RetType), Decls(Decls), Stmts(Stmts) {}
   
   const ParameterList &getParams() {
      return Params;
   }
   void setParams(ParameterList &PL) {
      Params = PL;
   }

   TypeDecl *getRetType() {
      return RetType;
   }
   void setRetType(TypeDecl *Ty) {
      RetType = Ty;
   }

   const DeclList &getDecls() {
      return Decls;
   }
   void setDecls(DeclList &DL) {
      Decls = DL;
   }

   const StmtList &getStmts() {
      return Stmts;
   }
   void setStmts(StmtList &SL) {
      Stmts = SL;
   }

public:
   static bool classof(const Decl *D) {
      return D->getKind() == DK_Func;
   }
};

class TypeDecl : public Decl {
protected:
   TypeDecl(DeclKind Kind, Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(Kind, EnclosingDecl, Loc, Name) {}

public:
   static bool classof(const Decl *D) {
      return D->getKind() >= DK_GlobalType &&
             D->getKind() <= DK_TypeEnd;
   }
};

class GlobalTypeDecl : public TypeDecl {
public:
   GlobalTypeDecl(Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : TypeDecl(DK_GlobalType, EnclosingDecl, Loc, Name) {}

public:
   static bool classof(const Decl *D) {
      return D->getKind() == DK_GlobalType;
   }
};

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
   
   bool getValue() {
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

   const OperatorInfo &getOperatorInfo() {
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

   const OperatorInfo &getOperatorInfo() {
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

   FunctionDecl *getDecl() {
      return Func;
   }

   const ExprList &getParams() {
      return Params;
   }

public:
   static bool classof(const Expr *E) {
      return E->getKind() == EK_Func;
   }
};

class Stmt {
public:
   enum StmtKind {
      SK_If,
      SK_While,
      SK_Return
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

class IfStmt : public Stmt {
   Expr *Cond;
   StmtList IfStmts;
   StmtList ElseStmts;

public:
   IfStmt(Expr *Cond, StmtList &IfStmts, StmtList &ElseStmts)
      : Stmt(SK_If), Cond(Cond), IfStmts(IfStmts), ElseStmts(ElseStmts) {}

   Expr *getCond() {
      return Cond;
   }

   const StmtList &getIfStmts() {
      return IfStmts;
   }

   const StmtList &getElseStmts() {
      return ElseStmts;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_If;
   }
};

class WhileStmt : public Stmt {
   Expr *Cond;
   StmtList Stmts;

public:
   WhileStmt(Expr *Cond, StmtList &Stmts)
      : Stmt(SK_While), Cond(Cond), Stmts(Stmts) {}

   Expr *getCond() {
      return Cond;
   }

   const StmtList &getWhileStmts() {
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
   
   Expr *getRetVal() {
      return RetVal;
   }

public:
   static bool classof(const Stmt *S) {
      return S->getKind() == SK_Return;
   } 
};

#endif