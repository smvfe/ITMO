project(libtesttask)

add_library(${PROJECT_NAME} include/${PROJECT_NAME}/${PROJECT_NAME}.hpp src/${PROJECT_NAME}.cpp)

target_include_directories(${PROJECT_NAME} PUBLIC include)
add_library(libtesttask::libtesttask ALIAS ${PROJECT_NAME})
