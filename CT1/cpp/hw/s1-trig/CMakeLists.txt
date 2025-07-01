cmake_minimum_required(VERSION 3.13)

set(CMAKE_PROJECT_TOP_LEVEL_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake)

project(calc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (TESTS_BUILD_TYPE MATCHES ASAN)
    set(COMPILE_OPTS -Wall -Wextra -Werror -pedantic -pedantic-errors
        -O1 -fsanitize=address -fno-omit-frame-pointer
        -fno-inline -fno-sanitize-recover=all)
    set(LINK_OPTS -fsanitize=address)
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

function(setup_warnings TARGET)
    target_compile_options(${TARGET} PUBLIC -Wno-unknown-warning-option)
    target_compile_options(${TARGET} PUBLIC -Wold-style-cast)
    target_compile_options(${TARGET} PUBLIC -Wnull-dereference)

    if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        target_compile_options(${TARGET} PUBLIC -Wduplicated-branches)
        target_compile_options(${TARGET} PUBLIC -Wduplicated-cond)
        target_compile_options(${TARGET} PUBLIC -Wsuggest-override)
        target_compile_options(${TARGET} PUBLIC -Wuseless-cast)
        target_compile_options(${TARGET} PUBLIC -Wreturn-local-addr)
    elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        target_compile_options(${TARGET} PUBLIC -Wreturn-stack-address)
        target_compile_options(${TARGET} PUBLIC -Wloop-analysis)
    elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
        target_compile_options(${TARGET} PUBLIC -Wreturn-stack-address)
        target_compile_options(${TARGET} PUBLIC -Wloop-analysis)
    endif()
endfunction(setup_warnings)

add_library(${PROJECT_NAME}_lib include/calc.hpp src/calc.cpp)
target_include_directories(${PROJECT_NAME}_lib PUBLIC include)
target_compile_options(${PROJECT_NAME}_lib PUBLIC ${COMPILE_OPTS})
target_link_options(${PROJECT_NAME}_lib PUBLIC ${LINK_OPTS})
setup_warnings(${PROJECT_NAME}_lib)

add_executable(${PROJECT_NAME} src/main.cpp)
target_compile_options(${PROJECT_NAME} PRIVATE ${COMPILE_OPTS})
target_link_options(${PROJECT_NAME} PRIVATE ${LINK_OPTS})
setup_warnings(${PROJECT_NAME})

# linking Main against the library
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib)

option(COMMON_TESTS          "Enable common tests"            ON)
option(FAC_LOG_VARIANT       "Enable tests for fac-log"       OFF)
option(FOLD_VARIANT          "Enable tests for fold"          OFF)
option(NUMBER_SYSTEM_VARIANT "Enable tests for number-system" OFF)
option(TRIG_VARIANT          "Enable tests for trig"          ON)

add_subdirectory(tests)
