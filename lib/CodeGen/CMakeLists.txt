set(LLVM_LINK_COMPONENTS support)

add_llox_library(lloxCodeGen
   CGCompilationUnit.cpp
   CGFunction.cpp
   CodeGenerator.cpp

   LINK_LIBS
   lloxSema
)