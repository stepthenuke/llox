#ifndef LLOX_LEXER_LEXER_H
#define LLOX_LEXER_LEXER_H

#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llox/Token/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

namespace llox {

class KeywordFilter {
   llvm::StringMap<tok::TokenKind> Table;

   void addKeyword(StringRef Keyword, tok::TokenKind TokenCode);

public:
   void addKeywords();
   tok::TokenKind getKeyword(StringRef Name, tok::TokenKind TokenIfNotFound);
};

class Lexer {
   SourceMgr &SrcMgr;
   StringRef Buf;
   const char *BufPtr;
   unsigned BufID;
   KeywordFilter Keywords;

public:
   Lexer(SourceMgr &SM);
   void getNextToken(Token &Tok);
   StringRef getBuffer() const;

private:
   void formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind);
   void setString(Token &Tok);
   void setNumber(Token &Tok);
   void setIdentifier(Token &Tok);
   void skipComment();
   SMLoc getLocation() const;
};

} // namespace llox

#endif