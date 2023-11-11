#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Casting.h"

#include <cctype>
#include <iostream>
#include <utility>

using llvm::SourceMgr;
using llvm::SMLoc;
using llvm::StringRef;
using llvm::cast;
using llvm::isa;
using llvm::dyn_cast;

namespace charinfo {

inline bool isASCII(char C) {
   return static_cast<unsigned char>(C) <= 127;
}

inline bool isDigit(char C) {
   return isASCII(C) && ('0' <= C && C <= '9');
}

inline bool isAlpha(char C) {
   return isASCII(C) && (('a' <= C && C <= 'z') || ('A' <= C && C <= 'Z') || C == '_');
}

inline bool isAlphanumeric(char C) {
   return isAlpha(C) || isDigit(C);
}

inline bool isVerticalWhitespace(char C) {
   return isASCII(C) && (C == '\n' || C == '\r');
}

inline bool isHorizontalWhitespace(char C) {
   return isASCII(C) && ( C == ' ' || C == '\t');
}

inline bool isWhitespace(char C) {
   return isVerticalWhitespace(C) || isHorizontalWhitespace(C);
}

} // namespace charinfo

namespace tok {

enum TokenKind : unsigned short {
// -------------------------------------------------------------
#define TOK(ID) ID,
#include "TokenKinds.def"
// -------------------------------------------------------------
   TOKEN_AMT
};

static const char *const TokNames[] = {
// -------------------------------------------------------------
#define TOK(ID) #ID,
#define KEYWORD(ID, SP) #ID,
#include "TokenKinds.def"
// -------------------------------------------------------------
   nullptr
};

const char *getTokenName(TokenKind Kind) {
   if (Kind < tok::TOKEN_AMT)
      return TokNames[Kind];
   llvm_unreachable("unknown TokenKind");
   return nullptr;
}

const char *getPunctuatorSpelling(TokenKind Kind) {
   switch (Kind) {
// -------------------------------------------------------------
#define PUNCTUATOR(ID, SP) case ID: return SP;
#define AMBIG_PUNCTUATOR(ID, SP) case ID: return SP;
#include "TokenKinds.def"
// -------------------------------------------------------------
      default: break;
   }
   return nullptr;
}

const char *getKeywordSpelling(TokenKind Kind) {
   switch (Kind) {
// -------------------------------------------------------------
#define KEYWORD(ID, SP) case kw_ ## ID: return SP;
#include "TokenKinds.def"
// -------------------------------------------------------------
   default: break;
   }
   return nullptr;
}

} // namespace tok


class Token {
   const char *Ptr;
   size_t Length;
   tok::TokenKind Kind;

public:
   void setPointer(const char *P) {
      Ptr = P;
   }

   const char *getPointer() const {
      return Ptr;
   }

   SMLoc getLocation() const {
      return SMLoc::getFromPointer(Ptr);
   }

   size_t getLength() const {
      return Length;
   }

   void setLength(size_t L) {
      Length = L;
   }

   tok::TokenKind getKind() const {
      return Kind;
   }

   void setKind(tok::TokenKind K) {
      Kind = K;
   }

   StringRef getName() const {
      return tok::getTokenName(Kind);
   }

   StringRef getIdentifier() const {
      assert(is(tok::identifier) && "not identifier");
      return StringRef(Ptr, Length);
   }

   StringRef getLiteral() const {
      assert(isOneOf({tok::double_literal, tok::string_literal}) && "not literal");
      return StringRef(Ptr, Length);
   }

   StringRef getTokenText() const {
      return StringRef(Ptr, Length);
   }

public:
   bool is(tok::TokenKind K) const {
      return K == Kind;
   }

   bool isNot(tok::TokenKind K) const {
      return !is(K);
   }

   bool isOneOf(std::initializer_list<tok::TokenKind> &&Kinds) const {
      for (auto K : Kinds) {
         if (is(K))
            return true;
      }
      return false;
   }
};

class KeywordFilter {
   llvm::StringMap<tok::TokenKind> Table;

   void addKeyword(StringRef Keyword, tok::TokenKind TokenCode) {
      Table.insert(std::make_pair(Keyword, TokenCode));
   }

public:
   void addKeywords() {
// -------------------------------------------------------------
#define KEYWORD(NAME, SP) \
   addKeyword(StringRef(#NAME), tok::kw_##NAME);
#include "TokenKinds.def"
// -------------------------------------------------------------
   }

   tok::TokenKind getKeyword(StringRef Name, tok::TokenKind TokenIfNotFound = tok::unknown) {
      // std::cout << "kw: " << Name.str() << std::endl;
      auto Res = Table.find(Name);
      if (Res != Table.end())
         return Res->second;
      return TokenIfNotFound;
   }
};

class Lexer {
   SourceMgr &SrcMgr;

   StringRef Buf;
   const char *BufPtr;
   unsigned BufID;

   KeywordFilter Keywords;

public:
   Lexer(SourceMgr &SM) 
      : SrcMgr{SM} {
      BufID = SrcMgr.getMainFileID();
      Buf = SrcMgr.getMemoryBuffer(BufID)->getBuffer();
      BufPtr = Buf.begin();
      Keywords.addKeywords();
   }

   void getNextToken(Token &Tok) {
      // skip all whitespaces
      while (*BufPtr && charinfo::isWhitespace(*BufPtr))
         ++BufPtr;

      // if we came to end -> it's eof
      if (!*BufPtr) {
         Tok.setKind(tok::eof);
         return;
      }

   // firstly we need to take out punctuators
      switch (*BufPtr) {
// -------------------------------------------------------------
#define CASE(c, tokenKind) \
   case (c)[0]: \
      formToken(Tok, BufPtr + 1, tokenKind); \
      return
// -------------------------------------------------------------
#define PUNCTUATOR(ID, SP) CASE(SP, tok::ID);
#include "TokenKinds.def"
// -------------------------------------------------------------
#undef CASE
// -------------------------------------------------------------
         case '/':
            if (*(BufPtr + 1) == '/')
               skipComment();
            else
               formToken(Tok, BufPtr + 1, tok::slash);
            return;
         case '!':
            if (*(BufPtr + 1) == '=')
               formToken(Tok, BufPtr + 2, tok::bang_equal);
            else
               formToken(Tok, BufPtr + 1, tok::bang);
            return;
         case '=':
            if (*(BufPtr + 1) == '=')
               formToken(Tok, BufPtr + 2, tok::equal_equal);
            else
               formToken(Tok, BufPtr + 1, tok::equal);
            return;
         case '>':
            if (*(BufPtr + 1) == '=')
               formToken(Tok, BufPtr + 2, tok::greater_equal);
            else
               formToken(Tok, BufPtr + 1, tok::greater);
            return;
         case '<':
            if (*(BufPtr + 1) == '=')
                  formToken(Tok, BufPtr + 2, tok::less_equal);
            else
               formToken(Tok, BufPtr + 1, tok::less); 
            return;
         default: break;
      }

      if (*BufPtr == '"') {
         setString(Tok);
         return;
      }

      // now we're left with numbers, identifiers, keywords
      if (charinfo::isDigit(*BufPtr)) { 
         setNumber(Tok);
         return;
      }

      if (charinfo::isAlpha(*BufPtr)) {
         setIdentifier(Tok);
         return;
      }

      Tok.setKind(tok::unknown);
   }

   StringRef getBuffer() const {
      return Buf;
   }

private:
   void formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind) {
      Tok.setKind(Kind);
      Tok.setPointer(BufPtr);
      size_t TokLen = TokEnd - BufPtr;
      Tok.setLength(TokLen);
      std::cout << StringRef(BufPtr, TokLen).str() << " : ";
      std::cout << Tok.getName().str() << "\n";
      BufPtr = TokEnd;
   }

   void setString(Token &Tok) {
      const char *Start = BufPtr;
      const char *End = BufPtr + 1;

      while (*End && *End != '"') {
         ++End;
      }
      
      if (*End != '"')
         llvm_unreachable("string is not closed");

      formToken(Tok, End + 1, tok::string_literal);
   }

   void setNumber(Token &Tok) {
      const char *Start = BufPtr;
      const char *End = BufPtr + 1;

      int DotCnt = 0;
      while (*End && (charinfo::isDigit(*End) || *End == '.')) {
         if (*End == '.')
            ++DotCnt;
         ++End;
      }
     
      if (DotCnt == 0 || DotCnt == 1) {
         formToken(Tok, End, tok::double_literal);
         return;
      }
      llvm_unreachable("not a valid number");

   }

   void setIdentifier(Token &Tok) {
      const char *Start = BufPtr;
      const char *End = BufPtr + 1;

      while (*End && charinfo::isAlphanumeric(*End)) {
         ++End;
      }

      StringRef Name(Start, End - Start);
      formToken(Tok, End, Keywords.getKeyword(Name, tok::identifier));
   }

   void skipComment() {
      while (*BufPtr && !charinfo::isVerticalWhitespace(*BufPtr))
         ++BufPtr;
   }

   SMLoc getLocation() const {
      return SMLoc::getFromPointer(BufPtr);
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

namespace op {

enum OperatorPrec {
   Prec_None,
   Prec_Assignment,
   Prec_Or,
   Prec_And,
   Prec_Equality,
   Prec_Comparison,
   Prec_Term,
   Prec_Factor,
   Prec_Unary,
   Prec_Call,
   Prec_Primary
};

OperatorPrec getUnaryPrec(tok::TokenKind K) {
   switch (K) {
// // -------------------------------------------------------------
#define UNARY_OPERATOR(ID, SP, PR) case (tok::ID): return PR;
#include "UnaryOperators.def"
// // -------------------------------------------------------------
   default: return Prec_None;
   }
};

OperatorPrec getBinaryPrec(tok::TokenKind K) {
   switch (K) {
// -------------------------------------------------------------
#define BINARY_OPERATOR(ID, SP, PR) case (tok::ID): return PR;
#include "BinaryOperators.def"
// -------------------------------------------------------------
   default: return Prec_None;
   }
};

inline bool isUnaryOp(tok::TokenKind K) {
   return getUnaryPrec(K) != Prec_None;
}

inline bool isBinaryOp(tok::TokenKind K) {
   return getBinaryPrec(K) != Prec_None;
}

inline bool isLogicalBinOp(tok::TokenKind K) {
   return getBinaryPrec(K) == Prec_Equality || getBinaryPrec(K) == Prec_Comparison;
}

}; // namespace op

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

class Scope {
   Scope *Parent;
   llvm::StringMap<Decl*> Symbols;

public:
   Scope(Scope *Parent = nullptr)
      : Parent(Parent) {}

   Scope *getParent() {
      return Parent;
   }

   bool insert(Decl *Declaration) {
      return Symbols.insert(std::make_pair(Declaration->getName(), Declaration)).second;
   }

   Decl *lookup(StringRef Name) {
      Scope *S = this;
      while (S) {
         const auto I = S->Symbols.find(Name);
         if (I != S->Symbols.end())
            return I->second;
         S = S->getParent();
      }
      return nullptr;
   }
};

class Sema {
   Scope *CurScope;
   Decl *CurDecl;

   TypeDecl *DoubleType;
   TypeDecl *BoolType;
   BoolLiteral *TrueLiteral;
   BoolLiteral *FalseLiteral;

   bool checkOperatorType(tok::TokenKind OpKind, TypeDecl *Ty) {
      switch (OpKind) {
      case tok::equal:
      case tok::bang_equal:
      case tok::equal_equal:
         return Ty == BoolType || Ty == DoubleType;
      case tok::greater:
      case tok::greater_equal:
      case tok::less:
      case tok::less_equal:
      case tok::plus:
      case tok::minus:
      case tok::star:
      case tok::slash:
         return Ty == DoubleType;
      case tok::bang:
      case tok::kw_and:
      case tok::kw_or:
         return Ty == BoolType;
      default:
         llvm_unreachable("unknown operator");
      }
      return false;
   }

public:
   void enterScope(Decl *D) {
      CurScope = new Scope(CurScope);
      CurDecl = D;
   }

   void leaveScope() {
      if (!CurScope)
         llvm_unreachable("can't leave non-existing scope");
      Scope *Parent = CurScope->getParent();
      delete CurScope;
      CurScope = Parent;
      CurDecl = CurDecl->getEnclosingDecl();
   }

public:
   Sema()
      : CurScope(new Scope()), CurDecl(nullptr) {
      DoubleType = new GlobalTypeDecl(CurDecl, SMLoc(), "double");
      BoolType = new GlobalTypeDecl(CurDecl, SMLoc(), "bool");
      TrueLiteral = new BoolLiteral(true, BoolType);
      FalseLiteral = new BoolLiteral(false, BoolType);
      CurScope->insert(DoubleType);
      CurScope->insert(BoolType);
   }

   FunctionDecl *actOnFunctionDecl(SMLoc Loc, StringRef Name) {
      FunctionDecl *FD = new FunctionDecl(CurDecl, Loc, Name);
      if (!CurScope->insert(FD))
         llvm_unreachable("Redeclaration of function");
      return FD;
   }

   void actOnFunctionParamList(FunctionDecl *FunDecl, ParameterList &Params, Decl *RetTy) {
      FunDecl->setParams(Params);
      auto CastedRetTy = llvm::dyn_cast_or_null<TypeDecl>(RetTy);
      if (!CastedRetTy && RetTy)
         llvm_unreachable("bad return type");
      else
         FunDecl->setRetType(CastedRetTy);
   }

   void actOnFunctionBlock(DeclList &Decls, FunctionDecl *FunDecl, DeclList &FunDecls, StmtList &FunStmts) {
      FunDecl->setDecls(FunDecls);
      FunDecl->setStmts(FunStmts);
      Decls.push_back(FunDecl);
   }

   void actOnFunctionParameters(ParameterList &Params, IdentList &ParIds, DeclList &ParTypes) {
      if (!CurScope)
         llvm_unreachable("current scope isn't set");
      assert(ParIds.size() == ParTypes.size());

      auto TypeIdx = ParTypes.begin();
      for (auto Idx = ParIds.begin(), E = ParIds.end(); Idx != E; ++Idx, ++TypeIdx) {
         if (auto *Ty = dyn_cast<TypeDecl>(*TypeIdx)) {
            SMLoc Loc = Idx->first;
            StringRef Name = Idx->second;
            ParameterDecl *ParDecl = new ParameterDecl(CurDecl, Loc, Name, Ty, true);
            if (CurScope->insert(ParDecl))
              Params.push_back(ParDecl);
            else
               llvm_unreachable("such parameter already exists");
         }
         else
            llvm_unreachable("not a type for parameter");
      }

   }

   Decl *actOnTypeIdent(Decl *Prev, SMLoc Loc, StringRef Name) {
      if (!Prev) {
         if (Decl *D = CurScope->lookup(Name))
            return D;
      }
      return nullptr;
   }

   void actOnVariableDecl(DeclList &Decls, Identifier Id, Decl *D) {
      if (!CurScope)
         llvm_unreachable("no current scope");
      
      if (TypeDecl *Ty = dyn_cast<TypeDecl>(D)) {
         SMLoc Loc = Id.first;
         StringRef Name = Id.second;
         VariableDecl *Decl = new VariableDecl(CurDecl, Loc, Name, Ty);
         if (CurScope->insert(Decl))
            Decls.push_back(Decl);
         else
            llvm_unreachable("variable already defined");
      }
      else 
         llvm_unreachable("no such type in var decl");
   }

   Expr *actOnDoubleLiteral(SMLoc Loc, StringRef Literal) {
      return new DoubleLiteral(Loc, llvm::APFloat(llvm::APFloat::IEEEdouble(), Literal), DoubleType);
   }

   Expr *actOnBoolLiteral(tok::TokenKind K) {
      if (K == tok::kw_true)
         return TrueLiteral;
      else
         return FalseLiteral;
   }

   Expr *actOnInfixExpr(Expr *Left, Expr *Right, const OperatorInfo &Op) {
      if (!Left)
         return Right;
      if (!Right)
         return Left;

      if (Left->getType() != Right->getType())
         llvm_unreachable("incompatible types");

      TypeDecl *Ty = Left->getType();
      if (!checkOperatorType(Op.getKind(), Ty))
         llvm_unreachable("incompatible operator for vals");

      bool RetTypeIsBool = op::isLogicalBinOp(Op.getKind());
      Ty = RetTypeIsBool ? BoolType : Ty;
      return new InfixExpr(Left, Right, Op, Ty);
   }

   Expr *actOnPrefixExpr(Expr *E, const OperatorInfo &Op) {
      if (!E)
         return nullptr;

      TypeDecl *Ty = E->getType();
      if (!checkOperatorType(Op.getKind(), Ty))
         llvm_unreachable("incompatible operator for val");
   
      return new PrefixExpr(E, Op, Ty);
   }
};

class Parser {
   Token CurTok;
   Lexer &Lex;
   Sema &Sem;

   using OperatorPrec = op::OperatorPrec;

public:
   Parser(Lexer &L, Sema &S)
      : Lex(L), Sem(S) {
      nextToken();
   }

public:
   void nextToken() {
      Lex.getNextToken(CurTok);
   }

   bool consumeToken(tok::TokenKind Kind) {
      if (CurTok.is(Kind)) {
         nextToken();
         return false;
      }
      return true;
   }

   bool consumeToken(std::initializer_list<tok::TokenKind> &&Kinds) {
      if (CurTok.isOneOf(std::move(Kinds))) {
         nextToken();
         return false;
      }
      return true;
   }

   OperatorPrec getBinOperatorPrec() {
      return op::getBinaryPrec(CurTok.getKind());
   } 

   OperatorPrec getUnOperatorPrec() {
      return op::getUnaryPrec(CurTok.getKind());
   }

   bool parseProgram();
   
   bool parseDecl(DeclList &Decls) {
      switch (CurTok.getKind()) {
      case tok::kw_var:
         return parseVariableDecl(Decls);
      case tok::kw_fun:
         return parseFunctionDecl(Decls);
      default:
         break;
      }
      return true;
   }
   
   bool parseVariableDecl(DeclList &Decls) {
      if (consumeToken(tok::kw_var))
         return true;
      if (CurTok.isNot(tok::identifier))
         return true;
      Identifier VarId {CurTok.getLocation(), CurTok.getIdentifier()};
      nextToken();

      if (consumeToken(tok::colon))
         return true;

      Decl *D;
      if (parseTypeIdent(D))
         return true;

      Sem.actOnVariableDecl(Decls, VarId, D);
      
      if (consumeToken(tok::semicolon))
         return true;

      return false;
   }

   bool parseTypeIdent(Decl *&D) {
      D = nullptr;
      if (CurTok.is(tok::kw_nil))
         return false;
      if (CurTok.isNot(tok::identifier))
         return true;
      D = Sem.actOnTypeIdent(D, CurTok.getLocation(),  CurTok.getIdentifier());
      nextToken();
      return false;
   }

   bool parseFunctionParameter(IdentList &ParIds, DeclList &ParTypes) {
      if (CurTok.is(tok::identifier))
         return true;
      ParIds.emplace_back(CurTok.getLocation(), CurTok.getIdentifier());
      nextToken();

      if (consumeToken(tok::colon))
         return true;

      Decl *D;
      if (parseTypeIdent(D))
         return true;
      ParTypes.push_back(D);

      return false;      
   }

   bool parseFunctionParameterList(ParameterList &Params) {
      if (CurTok.isNot(tok::identifier))
         return true;
      
      IdentList ParIds;
      DeclList ParTypes;
      if (parseFunctionParameter(ParIds, ParTypes))
         return true;
      while (CurTok.is(tok::comma)) {
         if (parseFunctionParameter(ParIds, ParTypes))
            return true;
      }
      Sem.actOnFunctionParameters(Params, ParIds, ParTypes);
      return false;
   }

   bool parseBlock(DeclList &Decls, StmtList &Stmts) {
      if (consumeToken(tok::l_brace))
         return true;
      
      while (CurTok.isNot(tok::eof) && CurTok.isNot(tok::r_brace))
         nextToken();

      if (consumeToken(tok::r_brace))
         return true;

      return false;
   }

   bool parseFunctionDecl(DeclList &Decls) {
      if (consumeToken(tok::kw_fun))
         return true;

      if (CurTok.isNot(tok::identifier))
         return true;
      Identifier FuncId {CurTok.getLocation(), CurTok.getIdentifier()};
      nextToken();

      // here we need to create function -> add it to scope
      // and enter its scope -> parse parameters and add them to funcs' scope
      
      FunctionDecl *FunDecl = Sem.actOnFunctionDecl(CurTok.getLocation(), CurTok.getIdentifier());
      Sem.enterScope(FunDecl);
      if (consumeToken(tok::l_paren))
         return true;

      ParameterList FunParams;
      if (parseFunctionParameterList(FunParams))
         return true;

      if (consumeToken(tok::r_paren))
         return true;
      if (consumeToken(tok::colon))
         return true;
      
      Decl *RetTy;
      if (parseTypeIdent(RetTy))
         return true;
       
      Sem.actOnFunctionParamList(FunDecl, FunParams, RetTy);

      DeclList FunDecls;
      StmtList FunStmts;
      if (parseBlock(FunDecls, FunStmts))
         return true;
      
      Sem.actOnFunctionBlock(Decls, FunDecl, FunDecls, FunStmts);
      return false;
   }

   bool parseIdentifierExpr(Expr *&E) {
      Identifier Id (CurTok.getLocation(), CurTok.getIdentifier());
      nextToken();

      // here we need to take the value from var
      // or call a function
      return true;
   }

   bool parseStringLiteral(Expr *&E) {
      return true;
   }

   bool parseParenExpr(Expr *&E) {
      consumeToken(tok::l_paren);
      if (parseExpr(E))
         return true;
      consumeToken(tok::r_paren);
      return false;
   }

   bool parseDoubleLiteral(Expr *&E) {
      if (CurTok.is(tok::double_literal)) {
         E = Sem.actOnDoubleLiteral(CurTok.getLocation(), CurTok.getLiteral());
         assert(isa<DoubleLiteral>(E));
         nextToken();
         return false;
      }
      return true;
   }

   bool parseBoolLiteral(Expr *&E) {
      if (CurTok.isOneOf({tok::kw_false, tok::kw_true})) {
         E = Sem.actOnBoolLiteral(CurTok.getKind());
         nextToken();
         return false;
      }
      return true;
   }

   bool parsePrimary(Expr *&E) {
      switch (CurTok.getKind()) {
      case tok::kw_false:
      case tok::kw_true:
         return parseBoolLiteral(E);  // TODO: do smth on false and true
      case tok::identifier:
         return parseIdentifierExpr(E);
      case tok::double_literal:
         return parseDoubleLiteral(E);
      case tok::string_literal:
         return parseStringLiteral(E);
      case tok::l_paren:
         return parseParenExpr(E);
      default:
         return false;
      }
   }

   bool parseExpr(Expr *&E) {
      if (parsePrefixExpr(E))
         return true;
      if (!E)
         return false;

      return parseInfixExpr(op::Prec_None, E);
   }
   
   bool parseInfixExpr(OperatorPrec LeftPrec, Expr *&Left) {
      while (true) {
         OperatorInfo BinOp(CurTok.getLocation(), CurTok.getKind());
         OperatorPrec BinOpPrec = BinOp.getBinPrec();
         
         if (BinOpPrec < LeftPrec)
            return false;
         
         nextToken(); // eat BinOp

         // there can be expr: 10 + -1 -> parsing + -> we find unary expr
         Expr *Right = nullptr;
         if (parsePrefixExpr(Right))
            return true;
         if (!Right)
            return false;

         OperatorPrec NextBinOpPrec = op::getBinaryPrec(CurTok.getKind());
         if (NextBinOpPrec > BinOpPrec) {
            OperatorPrec NewPrec = (op::Prec_Primary < static_cast<OperatorPrec>(BinOpPrec + 1)) 
                                   ? op::Prec_Primary : static_cast<OperatorPrec>(BinOpPrec + 1);
            if (parseInfixExpr(NewPrec, Right))
               return true;
            if (!Right)
               return false;
         }
         Left = Sem.actOnInfixExpr(Left, Right, BinOp);
      }
   }

   bool parsePrefixExpr(Expr *&E) {
      if (!op::isUnaryOp(CurTok.getKind())) {
         return parsePrimary(E);
      }

      OperatorInfo UnOp(CurTok.getLocation(), CurTok.getKind());
      nextToken();
      Expr *Operand = nullptr;
      if (parsePrefixExpr(Operand))
         return true;
      if (!Operand)
         return true;
      
      E = Sem.actOnPrefixExpr(Operand, UnOp);
      return false;
   }
   
};

static void printDecl(Decl *D) {
   if (!D) return;

   if (auto *V = dyn_cast<VariableDecl>(D)) {
      llvm::outs() << "Var: " << V->getName() << ", <" << V->getType()->getName() << ">\n";
   }
}

static void printDeclList(const DeclList &Decls) {
   for (auto D : Decls) {
      printDecl(D);
   }
}

static void printInfixAST(Expr *E) {
   if (!E) return;

   if (auto *I = dyn_cast<InfixExpr>(E)) {
      llvm::outs() << " ( " << tok::getTokenName(I->getOperatorInfo().getKind());
      llvm::outs() << " <" << I->getLeft()->getType()->getName() << ", " << I->getRight()->getType()->getName() << "> ";
      printInfixAST(I->getLeft());
      printInfixAST(I->getRight());
      llvm::outs() << " ) ";
   }
   else if (auto *U = dyn_cast<PrefixExpr>(E)) {
      llvm::outs() << " ( " << tok::getTokenName(U->getOperatorInfo().getKind());
      llvm::outs() << " <" << U->getExpr()->getType()->getName() << "> ";
      printInfixAST(U->getExpr());
      llvm::outs() << " ) ";
   } 
   else if (auto *D = dyn_cast<DoubleLiteral>(E)) {
      llvm::SmallVector<char, 16> Buffer;
      D->getValue().toString(Buffer);
      llvm::outs() << " d:" << Buffer << ",";
   }
   else if (auto *D = dyn_cast<BoolLiteral>(E)) {
      llvm::outs() << "b:" << ((D->getValue()) ? "true" : "false")  << ", ";
   }
}

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

      // Expr *E = nullptr;
      // llvm::outs() << Par.parseExpr(E) << "\n";
      // printInfixAST(E);

      DeclList Decls;
      while (!Par.parseDecl(Decls))
         llvm::outs() << 0 << " " << "\n";
      printDeclList(Decls);
   }

   return 0;
}