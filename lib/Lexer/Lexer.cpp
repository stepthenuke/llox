#include "llox/Lexer/Lexer.h"

namespace llox {

void KeywordFilter::addKeyword(StringRef Keyword, tok::TokenKind TokenCode) {
   Table.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords() {
// -------------------------------------------------------------
#define KEYWORD(NAME, SP) \
   addKeyword(StringRef(#NAME), tok::kw_##NAME);
#include "llox/Basic/TokenKinds.def"
// -------------------------------------------------------------
}

tok::TokenKind KeywordFilter::getKeyword(StringRef Name, tok::TokenKind TokenIfNotFound = tok::unknown) {
   // llvm::outs() << "kw: " << Name.str() << std::endl;
   auto Res = Table.find(Name);
   if (Res != Table.end())
      return Res->second;
   return TokenIfNotFound;
}

Lexer::Lexer(SourceMgr &SM) 
   : SrcMgr{SM} {
   BufID = SrcMgr.getMainFileID();
   Buf = SrcMgr.getMemoryBuffer(BufID)->getBuffer();
   BufPtr = Buf.begin();
   Keywords.addKeywords();
}

void Lexer::getNextToken(Token &Tok) {
   // skip all whitespaces
   while (*BufPtr && charinfo::isWhitespace(*BufPtr))
      ++BufPtr;

   // if we came to end -> it's eof
   if (!*BufPtr) {
      Tok.setKind(tok::eof);
      llvm::outs() << ": eof\n";
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
#include "llox/Basic/TokenKinds.def"
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

Token Lexer::peek(int n = 1) {
   const char *PrevBufPtr = BufPtr;
   Token PeekedTok;
   for (int i = 0; i < n; ++i)
      getNextToken(PeekedTok);
   BufPtr = PrevBufPtr;
   return PeekedTok;
}

StringRef Lexer::getBuffer() const {
   return Buf;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind) {
   Tok.setKind(Kind);
   Tok.setPointer(BufPtr);
   size_t TokLen = TokEnd - BufPtr;
   Tok.setLength(TokLen);
   llvm::outs() << StringRef(BufPtr, TokLen).str() << " : ";
   llvm::outs() << Tok.getName().str() << "\n";
   BufPtr = TokEnd;
}

void Lexer::setString(Token &Tok) {
   const char *End = BufPtr + 1;

   while (*End && *End != '"') {
      ++End;
   }
   
   if (*End != '"')
      llvm_unreachable("string is not closed");

   formToken(Tok, End + 1, tok::string_literal);
}

void Lexer::setNumber(Token &Tok) {
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

void Lexer::setIdentifier(Token &Tok) {
   const char *Start = BufPtr;
   const char *End = BufPtr + 1;

   while (*End && charinfo::isAlphanumeric(*End)) {
      ++End;
   }

   StringRef Name(Start, End - Start);
   formToken(Tok, End, Keywords.getKeyword(Name, tok::identifier));
}

void Lexer::skipComment() {
   while (*BufPtr && !charinfo::isVerticalWhitespace(*BufPtr))
      ++BufPtr;
}

SMLoc Lexer::getLocation() const {
   return SMLoc::getFromPointer(BufPtr);
}

} // namespace llox
