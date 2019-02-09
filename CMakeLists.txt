project(yap)
cmake_minimum_required(VERSION 3.13)
file(GLOB C_FILES "sources/*.c")

set(CMAKE_C_STANDARD 99)

include_directories(headers/)
add_executable(${PROJECT_NAME} main.c ${C_FILES})