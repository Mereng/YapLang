project(yap)
cmake_minimum_required(VERSION 3.13)

file(GLOB C_FILES "sources/*.c")
if (UNIX)
    file(GLOB PLATFORM_C_FILES "sources/unix/*.c")
endif (UNIX)


set(CMAKE_C_STANDARD 99)

include_directories(headers/)
add_executable(${PROJECT_NAME} main.c ${C_FILES} ${PLATFORM_C_FILES})