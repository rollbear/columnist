#!/bin/bash

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

git ls-files '*.[ch]pp'|xargs clang-format-18 --dry-run -Werror
exit $?
