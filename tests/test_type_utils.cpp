/*
 * columnist C++23 ECS (Entity Component System) library
 *
 * Copyright (C) Björn Fahller
 *
 *  Use, modification and distribution is subject to the
 *  Boost Software License, Version 1.0. (See accompanying
 *  file LICENSE_1_0.txt or copy at
 *  http://www.boost.org/LICENSE_1_0.txt)
 *
 * Project home: https://github.com/rollbear/columnist
 */

#include "columnist/type_utils.hpp"

#include <cstdio>
#include <string>

static_assert(columnist::is_specialization_v<std::basic_string, std::string>);
static_assert(!columnist::is_specialization_v<std::basic_string, int>);

template <columnist::specialization_of<std::basic_string> S>
class C {};

static_assert(columnist::type_index<int, char, bool, int, float, void> == 2);
static_assert(columnist::type_index<int, int> == 0);
static_assert(columnist::type_index<int, void, char, int> == 2);

static_assert(columnist::type_is_one_of<int, char, bool, int>);
static_assert(!columnist::type_is_one_of<int>);
static_assert(!columnist::type_is_one_of<int, void, char, double>);

int main() { std::puts("It compiled, so it passed"); }
