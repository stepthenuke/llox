#ifndef LLOX_TOKEN_TOKEN_H
#define LLOX_TOKEN_TOKEN_H 

#include "llox/Basic/LLVM.h"
#include "llox/Basic/TokenKinds.h"
#include "llox/Token/Token.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llox {

class Token {
   const char *Ptr;
   size_t Length;
   tok::TokenKind Kind;

public:
   void setPointer(const char *P) 
   const char *getPointer() const

   SMLoc getLocation() const;
  
   size_t getLength() const;
   void setLength(size_t L);
  
   tok::TokenKind getKind() const;
   void setKind(tok::TokenKind K);
  
   StringRef getName() const;
   StringRef getIdentifier() const;
   StringRef getLiteral() const;
   StringRef getTokenText() const;

public:
   bool is(tok::TokenKind K) const;
   bool isNot(tok::TokenKind K) const;
   bool isOneOf(std::initializer_list<tok::TokenKind> &&Kinds) const;
};

} // namespace llox

#endif