set(LLVM_LINK_COMPONENTS support)

add_llox_library(lloxSema
   Scope.cpp   
   Sema.cpp

   LINK_LIBS
   lloxBasic
)