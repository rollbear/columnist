cmake_minimum_required(VERSION 3.26)
project(columnist)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED yes)
set(CMAKE_CXX_EXTENSIONS  no)

find_package(Catch2 3 REQUIRED)


add_library(columnist INTERFACE)
add_library(columnist::columnist ALIAS columnist)
target_include_directories(
        columnist
        INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

install(TARGETS columnist
        EXPORT columnist-targets
        INCLUDES DESTINATION include)

add_subdirectory(tests)

