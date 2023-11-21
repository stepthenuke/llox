set(LLVM_LINK_COMPONENTS ${LLVM_TARGETS_TO_BUILD}
   Analysis CodeGen Core
   IPO AggressiveInstCombine InstCombine
   Instrumentation MC MCParser
   ObjCARCOpts Option ScalarOpts
   Support TransformUtils Vectorize
)

add_llox_tool(llox Driver.cpp SUPPORT_PLUGINS)

target_link_libraries(llox
   PRIVATE lloxBasic lloxCodeGen
   lloxParser lloxLexer lloxParser 
   lloxSema
)
