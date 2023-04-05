cmake_minimum_required(VERSION 2.6)

project(PUSHDOWN_TRANSLATOR)


add_definitions(-Wall -pedantic -g2 -O2 -std=c++0x)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -v -stdlib=libc++")

find_package(Boost COMPONENTS program_options REQUIRED)
include_directories("${Boost_INCLUDE_DIRS}")

add_subdirectory(src bin)

