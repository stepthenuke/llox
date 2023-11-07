#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/StringMap.h"
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

public:
   bool is(tok::TokenKind K) const {
      return K == Kind;
   }

   bool isNot(tok::TokenKind K) const {
      return !is(K);
   }

   bool isOneOf(std::initializer_list<tok::TokenKind> Kinds) const {
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