#include <store/store.hpp>
#include <store/functional.hpp>
#include <catch2/catch_test_macros.hpp>

TEST_CASE("remove_if using apply")
{

    table::store<int, std::string> s;
    s.insert(1, "one");
    s.insert(2, "two");
    s.insert(3, "three");
    s.insert(4, "four");
    s.insert(5, "five");
    s.insert(6, "six");
    {
        auto count = erase_if(s,
                              table::select<0>(
                                  store::apply([](auto t) { return t > 1; })));
        REQUIRE(count == 5);
        REQUIRE(s.size() == 1);
        auto [num, name] = *s.begin();
        REQUIRE(num == 1);
        REQUIRE(name == "one");
    }
}
