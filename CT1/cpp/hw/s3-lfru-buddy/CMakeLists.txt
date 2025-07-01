cmake_minimum_required(VERSION 3.24)

set(CMAKE_PROJECT_TOP_LEVEL_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake)

project(SmallTask3 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (TESTS_BUILD_TYPE MATCHES ASAN)
    set(COMPILE_OPTS -Wall -Wextra -Werror -pedantic -pedantic-errors
        -O1 -fsanitize=address -fno-omit-frame-pointer
        -fno-inline -fno-sanitize-recover=all)
    set(LINK_OPTS -fsanitize=address)
endif()
if (TESTS_BUILD_TYPE MATCHES LSAN)
    set(COMPILE_OPTS -Wall -Wextra -Werror -pedantic -pedantic-errors
        -O1 -fsanitize=leak -fno-omit-frame-pointer -fno-sanitize-recover=all)
    set(LINK_OPTS -fsanitize=leak)
endif()
if (TESTS_BUILD_TYPE MATCHES USAN)
    set(COMPILE_OPTS -Wall -Wextra -Werror -pedantic -pedantic-errors
        -O1 -fsanitize=undefined,float-cast-overflow,float-divide-by-zero
        -fno-omit-frame-pointer -fno-sanitize-recover=all
        -fsanitize-recover=alignment)
    set(LINK_OPTS -fsanitize=undefined,float-cast-overflow,float-divide-by-zero)
endif()

if (${USE_CLANG_TIDY})
    set(CMAKE_CXX_CLANG_TIDY clang-tidy)
endif()

add_subdirectory(libraries)

add_executable(main src/main.cpp)
target_link_libraries(main PRIVATE acp::acp)
