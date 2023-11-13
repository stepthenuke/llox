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

} // namespace llox

#endif