project(${TASK_VARIANT})

add_library(${TASK_VARIANT} include/tree/Tree.hpp src/Tree.cpp)

target_include_directories(${TASK_VARIANT} PUBLIC include)

add_library(tree::${TASK_VARIANT} ALIAS ${TASK_VARIANT})

enable_testing()
find_package(GTest REQUIRED)
include(GoogleTest)

add_executable(runUnitTests tests/main.cpp)
target_compile_definitions(runUnitTests PRIVATE TASK_VARIANT=${TASK_VARIANT})
target_link_libraries(runUnitTests PRIVATE GTest::GTest tree::${TASK_VARIANT})
gtest_discover_tests(runUnitTests)

if(COMPILE_OPTS)
    target_compile_options(${TASK_VARIANT} PUBLIC ${COMPILE_OPTS})
    target_link_options(${TASK_VARIANT} PUBLIC ${LINK_OPTS})

    target_compile_options(runUnitTests PUBLIC ${COMPILE_OPTS})
    target_link_options(runUnitTests PUBLIC ${LINK_OPTS})
endif()
