project(wordnet)

add_library(${PROJECT_NAME}
    include/wordnet/Wordnet.hpp src/Wordnet.cpp
)

target_include_directories(${PROJECT_NAME} PUBLIC include)

add_library(wordnet::wordnet ALIAS ${PROJECT_NAME})

enable_testing()
find_package(GTest REQUIRED)
include(GoogleTest)

add_executable(runUnitTests tests/test.cpp tests/test_iterator.hpp)
target_link_libraries(runUnitTests PRIVATE GTest::GTest wordnet::wordnet)
gtest_discover_tests(runUnitTests)

file(GLOB ETC_FILES RELATIVE ${PROJECT_SOURCE_DIR} ${PROJECT_SOURCE_DIR}/etc/*)

add_custom_command(
    OUTPUT ${ETC_FILES}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${PROJECT_SOURCE_DIR}/etc ${CMAKE_CURRENT_BINARY_DIR}/test/etc
    DEPENDS ${ETC_FILES}
    COMMENT "Copying test data")

add_custom_target(
    etc DEPENDS ${ETC_FILES})

add_dependencies(runUnitTests etc)

if(COMPILE_OPTS)
    target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTS})
    target_link_options(${PROJECT_NAME} PUBLIC ${LINK_OPTS})

    target_compile_options(runUnitTests PUBLIC ${COMPILE_OPTS})
    target_link_options(runUnitTests PUBLIC ${LINK_OPTS})
endif()
