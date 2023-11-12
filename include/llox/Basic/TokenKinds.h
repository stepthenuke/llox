#ifndef LLOX_BASIC_TOKENKINDS_H
#define LLOX_BASIC_TOKENKINDS_H

namespace llox {
namespace tok {

enum TokenKind : unsigned short {
// -------------------------------------------------------------
#define TOK(ID) ID,
#include "llox/Basic/TokenKinds.def"
// -------------------------------------------------------------
   TOKEN_AMT
};

const char *getTokenName(TokenKind Kind);
const char *getPunctuatorSpelling(TokenKind Kind);
const char *getKeywordSpelling(TokenKind Kind);

} // namespace tok

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

OperatorPrec getUnaryPrec(tok::TokenKind K);
OperatorPrec getBinaryPrec(tok::TokenKind K);
inline bool isUnaryOp(tok::TokenKind K);
inline bool isBinaryOp(tok::TokenKind K);
inline bool isLogicalBinOp(tok::TokenKind K);

}; // namespace op

} // namespace llox

#endif