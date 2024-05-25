#include "columnist/functional.hpp"
#include "columnist/table.hpp"

#include <catch2/catch_test_macros.hpp>

#include <algorithm>
#include <ranges>

TEST_CASE("erase_if using apply")
{

    columnist::table<int, std::string> s;
    s.insert(1, "one");
    s.insert(2, "two");
    s.insert(3, "three");
    s.insert(4, "four");
    s.insert(5, "five");
    s.insert(6, "six");
    WHEN("erasing by index")
    {
        auto count
            = erase_if(s, columnist::select<0>(columnist::apply([](auto t) {
                           return t > 1;
                       })));
        THEN("matching objects are erased")
        {
            REQUIRE(count == 5);
            REQUIRE(s.size() == 1);
            auto [num, name] = *s.begin();
            REQUIRE(num == 1);
            REQUIRE(name == "one");
        }
    }
    AND_WHEN("erasing ty type")
    {
        auto count
            = erase_if(s, columnist::select<int>(columnist::apply([](auto t) {
                           return t > 1;
                       })));
        THEN("matching objects are erased")
        {
            REQUIRE(count == 5);
            REQUIRE(s.size() == 1);
            auto [num, name] = *s.begin();
            REQUIRE(num == 1);
            REQUIRE(name == "one");
        }
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

TEST_CASE("ranges for_each on select range")
{
    columnist::table<int, long> s;
    s.insert(1, 10);
    s.insert(2, 20);
    s.insert(3, 30);
    s.insert(4, 40);
    s.insert(5, 50);
    WHEN("selected by index")
    {
        std::vector<int> v;
        std::ranges::for_each(s | columnist::select<0>(),
                              columnist::apply([](int& x) { x = -x; }));
        std::ranges::for_each(s,
                              columnist::select<0>(columnist::apply(
                                  [&v](int x) { v.push_back(x); })));
        THEN("the selected index is used in function calls")
        {
            REQUIRE(v == std::vector{ -1, -2, -3, -4, -5 });
        }
    }
    WHEN("selected by type")
    {
        std::vector<int> v;
        std::ranges::for_each(s | columnist::select<int>(),
                              columnist::apply([](int& x) { x = -x; }));
        std::ranges::for_each(s,
                              columnist::select<int>(columnist::apply(
                                  [&v](int x) { v.push_back(x); })));
        THEN("the selected index is used in function calls")
        {
            REQUIRE(v == std::vector{ -1, -2, -3, -4, -5 });
        }
    }
}
