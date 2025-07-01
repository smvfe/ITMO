project(runUnitTests LANGUAGES CXX)

find_package(GTest REQUIRED)

add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE calc_lib GTest::GTest)
target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTS})
target_link_options(${PROJECT_NAME} PUBLIC ${LINK_OPTS})
setup_warnings(${PROJECT_NAME})

if(COMMON_TESTS)
    message(STATUS "COMMON_TESTS enabled")
    target_compile_definitions(${PROJECT_NAME} PRIVATE COMMON_TESTS)
endif()

if(FAC_LOG_VARIANT)
    message(STATUS "FAC_LOG_VARIANT enabled")
    target_compile_definitions(${PROJECT_NAME} PRIVATE FAC_LOG_VARIANT)
endif()

if(FOLD_VARIANT)
    message(STATUS "FOLD_VARIANT enabled")
    target_compile_definitions(${PROJECT_NAME} PRIVATE FOLD_VARIANT)
endif()

if(NUMBER_SYSTEM_VARIANT)
    message(STATUS "NUMBER_SYSTEM_VARIANT enabled")
    target_compile_definitions(${PROJECT_NAME} PRIVATE NUMBER_SYSTEM_VARIANT)
endif()

if(TRIG_VARIANT)
    message(STATUS "TRIG_VARIANT enabled")
    target_compile_definitions(${PROJECT_NAME} PRIVATE TRIG_VARIANT)
endif()
