cmake_minimum_required(VERSION 3.23)
project(GameOfLife C)

set(CMAKE_C_STANDARD 99)

find_package(SDL2 REQUIRED SDL2)
include_directories(GameOfLife ${SDL2_INCLUDE_Dirs})

add_executable(GameOfLife main.c Logic.c Logic.h GUI.c GUI.h)
target_link_libraries(GameOfLife ${SDL2_LIBRARIES})

