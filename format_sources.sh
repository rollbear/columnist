#/bin/bash

git ls-files '*.[ch]pp'|xargs clang-format-18 -i
