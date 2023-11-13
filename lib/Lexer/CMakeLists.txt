set(LLVM_LINK_COMPONENTS support)

add_llox_library(lloxLexer
   Lexer.cpp

   LINK_LIBS
   lloxBasic
   lloxToken
)