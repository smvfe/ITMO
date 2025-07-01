project(lfru-buddy)

add_library(${PROJECT_NAME}
    include/acp/Allocator.hpp src/Allocator.cpp
    include/acp/Pool.hpp      src/Pool.cpp
    include/acp/Cache.hpp
)

target_include_directories(${PROJECT_NAME} PUBLIC include)

add_library(acp::acp ALIAS ${PROJECT_NAME})

enable_testing()
find_package(GTest REQUIRED)
include(GoogleTest)

add_executable(runUnitTests tests/test_alloc.cpp tests/test_cache.cpp)
target_link_libraries(runUnitTests PRIVATE GTest::GTest acp::acp)
gtest_discover_tests(runUnitTests)

if(COMPILE_OPTS)
    target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTS})
    target_link_options(${PROJECT_NAME} PUBLIC ${LINK_OPTS})

    target_compile_options(runUnitTests PUBLIC ${COMPILE_OPTS})
    target_link_options(runUnitTests PUBLIC ${LINK_OPTS})
endif()

if((NOT TESTS_BUILD_TYPE) OR (TESTS_BUILD_TYPE MATCHES NONE))
    find_package(jemalloc REQUIRED)
    target_compile_definitions(runUnitTests PRIVATE JEMALLOC)
    target_link_libraries(runUnitTests PRIVATE jemalloc::jemalloc)
endif()
