#include <columnist/functional.hpp>
#include <columnist/table.hpp>

#include <catch2/catch_test_macros.hpp>

#include <ranges>

TEST_CASE("remove_if using apply")
{

    columnist::table<int, std::string> s;
    s.insert(1, "one");
    s.insert(2, "two");
    s.insert(3, "three");
    s.insert(4, "four");
    s.insert(5, "five");
    s.insert(6, "six");
    {
        auto count
            = erase_if(s, columnist::select<0>(columnist::apply([](auto t) {
                           return t > 1;
                       })));
        REQUIRE(count == 5);
        REQUIRE(s.size() == 1);
        auto [num, name] = *s.begin();
        REQUIRE(num == 1);
        REQUIRE(name == "one");
    }
}

TEST_CASE("ranges filter and transform")
{
    columnist::table<int, int> s;
    s.insert(1, 10);
    s.insert(2, 20);
    s.insert(3, 30);
    s.insert(4, 40);
    s.insert(5, 50);

    auto odd_diffs = s
                   | std::ranges::views::filter(columnist::select<0>(
                       columnist::apply([](int x) { return x & 1; })))
                   | std::ranges::views::transform(columnist::select<1, 0>(
                       columnist::apply(std::minus{})))
                   | std::ranges::to<std::vector<int>>();
    REQUIRE(odd_diffs == std::vector{ 9, 27, 45 });
}
