#
# columnist C++23 ECS (Entity Component System) library
#
# Copyright (C) Björn Fahller
#
#  Use, modification and distribution is subject to the
#  Boost Software License, Version 1.0. (See accompanying
#  file LICENSE_1_0.txt or copy at
#  http://www.boost.org/LICENSE_1_0.txt)
#
# Project home: https://github.com/rollbear/columnist
#


cmake_minimum_required(VERSION 3.22)
project(columnist)

option(unittest, "Enable unittest" no)

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

if (unittest)
    add_subdirectory(tests)
endif()
