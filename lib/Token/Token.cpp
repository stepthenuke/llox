#include "llox/Token/Token.h"

namespace llox {

void Token::setPointer(const char *P) {
   Ptr = P;
}

const char *Token::getPointer() const {
   return Ptr;
}

SMLoc Token::getLocation() const {
   return SMLoc::getFromPointer(Ptr);
}

size_t Token::getLength() const {
   return Length;
}

void Token::setLength(size_t L) {
   Length = L;
}

tok::TokenKind Token::getKind() const {
   return Kind;
}

void Token::setKind(tok::TokenKind K) {
   Kind = K;
}

StringRef Token::getName() const {
   return tok::getTokenName(Kind);
}

StringRef Token::getIdentifier() const {
   assert(is(tok::identifier) && "not identifier");
   return StringRef(Ptr, Length);
}

StringRef Token::getLiteral() const {
   assert(isOneOf({tok::double_literal, tok::string_literal}) && "not literal");
   return StringRef(Ptr, Length);
}

StringRef Token::getTokenText() const {
   return StringRef(Ptr, Length);
}

bool Token::is(tok::TokenKind K) const {
   return K == Kind;
}

bool Token::isNot(tok::TokenKind K) const {
   return !is(K);
}

bool Token::isOneOf(std::initializer_list<tok::TokenKind> &&Kinds) const {
   for (auto K : Kinds) {
      if (is(K))
         return true;
   }
   return false;
}

} // namespace llox