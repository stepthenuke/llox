#include "llox/Basic/TokenKinds.h"

namespace llox {
namespace tok {

static const char *const TokNames[] = {
// -------------------------------------------------------------
#define TOK(ID) #ID,
#define KEYWORD(ID, SP) #ID,
#include "llox/Basic/TokenKinds.def"
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
#include "llox/Basic/TokenKinds.def"
// -------------------------------------------------------------
      default: break;
   }
   return nullptr;
}

const char *getKeywordSpelling(TokenKind Kind) {
   switch (Kind) {
// -------------------------------------------------------------
#define KEYWORD(ID, SP) case kw_ ## ID: return SP;
#include "llox/Basic/TokenKinds.def"
// -------------------------------------------------------------
   default: break;
   }
   return nullptr;
}

} // namespace tok

namespace op {

OperatorPrec getUnaryPrec(tok::TokenKind K) {
   switch (K) {
// // -------------------------------------------------------------
#define UNARY_OPERATOR(ID, SP, PR) case (tok::ID): return PR;
#include "llox/Basic/UnaryOperators.def"
// // -------------------------------------------------------------
   default: return Prec_None;
   }
};

OperatorPrec getBinaryPrec(tok::TokenKind K) {
   switch (K) {
// -------------------------------------------------------------
#define BINARY_OPERATOR(ID, SP, PR) case (tok::ID): return PR;
#include "llox/Basic/BinaryOperators.def"
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

} // namespace llox
