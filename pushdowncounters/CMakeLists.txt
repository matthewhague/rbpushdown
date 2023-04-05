cmake_minimum_required(VERSION 2.6)

project(PUSHDOWN_COUNTERS)

add_definitions(-g -O2 -std=c++0x)

find_package(Boost COMPONENTS program_options REQUIRED)
include_directories("${Boost_INCLUDE_DIRS}")

add_subdirectory(src bin)
