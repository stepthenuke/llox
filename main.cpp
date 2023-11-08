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
      assert(is(tok::double_literal) && "not literal");
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
   GlobalTypeDecl(Delc *EnclosingDecl, SMLoc Loc, StringRef Name)
      : TypeDecl(DK_GlobalType, EnclosingDecl, Loc, Name) {}

public:
   static bool classof(const Decl *D) {
      return D->getKind() == DK_GlobalType;
   }
}


class OperatorInfo {
   SMLoc Loc;
   uint32_t Kind : 16;
   uint32_t IsUnspecified : 1;

public:
   OperatorInfo()
      : Loc(), Kind(tok::unknown), IsUnspecified(true) {}

   OperatorInfo(SMLoc Loc, tok::TokenKind Kind, bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {}
   
   SMLoc getLocation() const {
      return Loc;
   }

   tok::TokenKind getKind() const {
      return static_cast<tok::TokenKind>(Kind);
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
      return E->getKind() == EK_Double;
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

class OperatorPrecTable {
   enum OperatorPrecedence {
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
      // PREC_NONE,
      // PREC_ASSIGNMENT,  // =
      // PREC_OR,          // or
      // PREC_AND,         // and
      // PREC_EQUALITY,    // == !=
      // PREC_COMPARISON,  // < > <= >=
      // PREC_TERM,        // + -
      // PREC_FACTOR,      // * /
      // PREC_UNARY,       // ! -
      // PREC_CALL,        // . ()
      // PREC_PRIMARY
   };

   llvm::StringMap<OperatorPrecedence> Table;
   // fill the map somewhere and use it

public:
   OperatorPrecTable() {
      Table.insert(std::make_pair("=", Prec_Assignment));
      Table.insert(std::make_pair("or", Prec_Or));
      Table.insert(std::make_pair("and", Prec_And));
      Table.insert(std::make_pair("==", Prec_Equality));
      Table.insert(std::make_pair("!=", Prec_Equality));
      Table.insert(std::make_pair("<", Prec_Comparison));
      Table.insert(std::make_pair("<=", Prec_Comparison));
      Table.insert(std::make_pair(">", Prec_Comparison));
      Table.insert(std::make_pair(">=", Prec_Comparison));
      Table.insert(std::make_pair("+", Prec_Term));
      Table.insert(std::make_pair("-", Prec_Term));
      Table.insert(std::make_pair("*", Prec_Factor));
      Table.insert(std::make_pair("/", Prec_Factor));
      Table.insert(std::make_pair("!", Prec_Unary));
      Table.insert(std::make_pair("-", Prec_Unary));
      Table.insert(std::make_pair(".", Prec_Call));
   }

   OperatorPrecedence getPrecedence(StringRef OpName) {
      auto Res = Table.find(OpName);
      if (Res != Table.end())
         return Res->second;

      return Prec_None; 
   }
};

class Scope {
   Scope *Parent;
   StringMap<Decl*> Symbols;

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
      Scope *S = T
      while (S) {
         const auto I = S->Symbols.find(Name);
         if (I != S->Symbols.end())
            return I->second;
         S = S->getParent();
      }
   }
};

class Sema {
   Scope *CurScope;
   Decl *CurDecl;

   TypeDecl *DoubleType;
   TypeDecl *BoolType;
   BoolLiteral *TrueLiteral;
   BoolLiteral *FalseLiteral;
   
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

};

class Parser {
   Token CurTok;
   Lexer &Lex;

   OperatorPrecTable &OpPrec;
   using Precedence = OperatorPrecTable::OperatorPrecedence;

   Parser(Lexer &L)
      : Lex(L) {}

private:
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

   bool checkToken(tok::TokenKind Kind) {
      return !CurTok.is(Kind);
   }

   bool checkToken(std::initializer_list<tok::TokenKind> &&Kinds) {
      return !CurTok.isOneOf(std::move(Kinds));
   }

   Precedence getOperatorPrec() {
      StringRef OpName = CurTok.getTokenText();
      return OpPrec.getPrecedence(OpName);
   }

   bool parseProgram();
   bool parseDecl();
   bool parseVariableDecl();
   bool parseFunctionDecl();
   bool parseParameterDecl();

   // primary        → "true" | "false" | {"nil"} | "this"
   //             | NUMBER | STRING | IDENTIFIER | "(" expression ")"
   //             {| "super" "." IDENTIFIER} ;
   bool parsePrimary(Expr *&E) {
      switch (CurTok) {
      case tok::kw_true:
         E = new BoolLiteral(true, nullptr);
         return false;
      case tok::kw_false:
         E = new BoolLiteral(false, nullptr);
         return false;
      case tok::identifier:
         return parseIdentifierExpr(E);
      case tok::double_literal:
         return parseNumberExpr(E);
      case tok::string_literal:
         return parseStringExpr(E);
      case tok::l_paren:
         return parseParenExpr(E);
      default:
         break;
      }
      return false;
   } 

   bool parseIdentifier(Expr *&E) {
      // TODO
      // if "(" -> call function
      // else take value from identifier
      // also this can be a kw -> error or what?
      return true;
   }

   bool parseNumberExpr(Expr *&E) {
      if (CurTok.is(tok::double_literal)) {
         // E = Sem.actOnDoubleLiteral(CurTok.getLocation(), CurTok.getLiteral());
         StringRef Lit = CurTok.getTokenText();
         llvm::APFloat Value (std::stod(Lit.data(), Lit.size()));
         E = new DoubleLiteral(CurTok.getLocation(), Value, nullptr);
         nextToken();
      }
      return false;
   }

   bool parseStringExpr(Expr *&E) {
      // TODO
      if (CurTok.is(tok::string_literal)) {
         // E = Sem.actOnStringLiteral(); // #TODO
         nextToken();
      }
      return true;
   }

   bool parseParenExpr(Expr *&E) {
      consumeToken(tok::l_paren);
      if (parseExpression(E))
         return true;
      consumeToken(tok::r_paren);
      return false;
   }

   bool parseExpression(Expr *&E) {
      if (parsePrimary(E))
         return true;
   
      return false;
   }

   bool parseInfixExpr(Precedence LeftPrec, Expr *&Left) {
      while (true) {
         Precedence TokPrec = getOperatorPrec();
         if (TokPrec < LeftPrec)
            return false;

         Token BinOp = CurTok;
         nextToken();

         // if we're here -> we have binop that needs to be parsed
         Expr *Right = nullptr;
         if (parsePrimary(Right))
            return true;

         Precedence RightPrec = getOperatorPrec();
         if (LeftPrec < RightPrec) {
            if (parseInfixExpr(LeftPrec + 1, Right))
               return true;
         }
         
         OperatorInfo OpInfo (BinOp.getLocation(), BinOp.getKind());
         Left = new InfixExpr(Left, Right, OpInfo, nullptr);
      }
   }
   
};


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

      Token Tok;
      Tok.setKind(tok::unknown);
      while (Tok.isNot(tok::eof)) {
         Lex.getNextToken(Tok);
      }
   }

   return 0;
}