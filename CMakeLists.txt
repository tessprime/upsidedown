cmake_minimum_required(VERSION 3.16)
project(upsidedown)

set(REQUIRED_LLVM_MAJOR 16)

# Prefer a known LLVM install prefix if the user didn't provide one.
# Linux apt installs typically live here:
if (EXISTS "/usr/lib/llvm-${REQUIRED_LLVM_MAJOR}")
  list(PREPEND CMAKE_PREFIX_PATH "/usr/lib/llvm-${REQUIRED_LLVM_MAJOR}")
endif()

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")

message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
find_package(Clang REQUIRED CONFIG)

message(STATUS "Using ClangConfig.cmake in: ${Clang_DIR}")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(upsidedown src/main.cpp)

# Add linker flags to avoid duplicate symbol errors
if(UNIX AND NOT APPLE)
  target_link_options(upsidedown PRIVATE "-Wl,--allow-multiple-definition")
endif()

llvm_map_components_to_libnames(LLVM_LIBS support)
target_link_libraries(upsidedown
  PRIVATE
  clangTooling
  clangFrontend
  clangAST
  clangASTMatchers
  clangBasic
  clangRewrite
  clangIndex
  ${LLVM_LIBS}
)
