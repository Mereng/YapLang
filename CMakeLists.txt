project(yap)
cmake_minimum_required(VERSION 3.13)
file(GLOB C_FILES "src/*.c")

set(CMAKE_C_STANDARD 99)


add_executable(${PROJECT_NAME} ${C_FILES})