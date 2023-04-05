
add_executable(pushdown_counters main.cpp)
target_link_libraries(pushdown_counters structures-lib 
                                        translators-lib 
                                        writers-lib
                                        ${Boost_PROGRAM_OPTIONS_LIBRARY})
