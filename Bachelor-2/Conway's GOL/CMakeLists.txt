cmake_minimum_required(VERSION 3.23)
project(Pre_project C)

set(CMAKE_C_STANDARD 99)

find_package(SDL2 REQUIRED SDL2)
include_directories(<project_naam> ${SDL2_INCLUDE_DIRS})

add_executable(Pre_project main.c)
target_link_libraries(<project_naam> ${SDL2_LIBRARIES})
