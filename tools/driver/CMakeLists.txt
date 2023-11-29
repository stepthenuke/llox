set(LLVM_LINK_COMPONENTS ${LLVM_TARGETS_TO_BUILD}
   AggressiveInstCombine Analysis AsmParser BitWriter
   CodeGen Core Coroutines IPO IRReader InstCombine
   Instrumentation MC MCParser ObjCARCOpts Option Remarks
   ScalarOpts Support Target TransformUtils Vectorize Passes
)

add_llox_tool(llox Driver.cpp SUPPORT_PLUGINS)

target_link_libraries(llox
   PRIVATE lloxBasic lloxCodeGen
   lloxParser lloxLexer lloxParser 
   lloxSema
)
