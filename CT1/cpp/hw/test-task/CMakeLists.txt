cmake_minimum_required(VERSION 3.24)

set(CMAKE_PROJECT_TOP_LEVEL_INCLUDES conan.cmake)

project(TestTask LANGUAGES CXX)

option(USE_CLANG_TIDY "Run clang-tidy while building" OFF)
option(BUILD_TESTS "Build test" ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(GTest REQUIRED)

if (USE_CLANG_TIDY)
    set(CMAKE_CXX_CLANG_TIDY clang-tidy "-checks=-*,readability-*,llvm-header-guard")
endif()

add_subdirectory(libtesttask)

if(BUILD_TESTS)
    add_subdirectory(tests)
endif()

add_executable(TestTask src/main.cpp)
target_link_libraries(TestTask PRIVATE libtesttask::libtesttask)
