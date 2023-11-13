set(LLVM_LINK_COMPONENTS support)

add_llox_library(lloxToken
   Token.cpp

   LINK_LIBS
   lloxBasic
)