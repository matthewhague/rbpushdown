
add_library(translators-lib pds2cfg.cpp cfg2pres.cpp problem2pres.cpp)
target_link_libraries(translators-lib structures-lib)
