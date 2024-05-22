#include <columnist/type_utils.hpp>
#include <string>
#include <cstdio>

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

int main()
{
    std::puts("It compiled, so it passed");
}
