
add_executable(pushdown_translater main.cpp)
target_link_libraries(pushdown_translater ${Boost_PROGRAM_OPTIONS_LIBRARY}
                                          structures-lib
                                          parser-lib
                                          translators-lib
                                          writers-lib
                                          tools-lib
                                          codeoptimisers-lib
                                          pdsoptimisers-lib)
