set(LLVM_LINK_COMPONENTS support)

add_llox_library(lloxParser
   Parser.cpp

   LINK_LIBS
   lloxBasic
   lloxToken
   lloxLexer
)