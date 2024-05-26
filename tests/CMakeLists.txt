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

add_executable(test_table test_table.cpp)
target_link_libraries(test_table
        columnist::columnist
        Catch2::Catch2WithMain)

add_executable(test_functional test_functional.cpp)
target_link_libraries(test_functional columnist::columnist Catch2::Catch2WithMain)

add_executable(test_combined test_combined.cpp)
target_link_libraries(test_combined columnist::columnist Catch2::Catch2WithMain)

add_executable(test_type_utils test_type_utils.cpp)
target_link_libraries(test_type_utils columnist::columnist Catch2::Catch2WithMain)

add_executable(fuzz_table fuzz_table.cpp)
target_compile_options(fuzz_table PRIVATE "-fsanitize=fuzzer,address,undefined")
target_link_options(fuzz_table PRIVATE "-fsanitize=fuzzer,address,undefined")
target_link_libraries(fuzz_table columnist::columnist)
