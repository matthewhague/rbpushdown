
find_package(BISON)
find_package(FLEX)

BISON_TARGET(Parser parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp COMPILE_FLAGS -d)

FLEX_TARGET(Scanner tokens.l ${CMAKE_CURRENT_BINARY_DIR}/tokens.cpp)

ADD_FLEX_BISON_DEPENDENCY(Scanner Parser)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

add_library(parser-lib ${BISON_Parser_OUTPUTS} ${FLEX_Scanner_OUTPUTS})

target_link_libraries(parser-lib structures-lib)
