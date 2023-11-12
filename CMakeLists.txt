cmake_minimum_required()(VERSION 3.23.0)
project(llox)

find_package(LLVM_REQUIRED HINTS "LLVM_CMAKE_PATH")
list(APPEND CMAKE_MODULE_PAtH ${LLVM_DIR})

include(AddLLVM)
include(HandleLLVMOptions)

include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_INCLUDE_DIR}")
link_directories("${LLVM_LIBRARY_DIR}")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
   message(FATAL_ERROR "Build must not be in-source")
endif()

include(AddLlox)

include_directories(BEFORE "${CMAKE_CURRENT_BINARY_DIR}/include"
                           "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_subdirectory(lib)
add_subdirectory(tools)