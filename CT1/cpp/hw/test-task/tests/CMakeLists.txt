project(runUnitTests LANGUAGES CXX)

add_executable(${PROJECT_NAME} src/runUnitTests.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE libtesttask::libtesttask GTest::gtest_main)
