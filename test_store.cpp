#include <catch2/catch_test_macros.hpp>
#include <store/store.hpp>

#include <iostream>
#include <map>
#include <set>

using table::store;

TEST_CASE("a default constructed store is empty")
{
    store<int> s;
    REQUIRE(s.empty());
    REQUIRE(s.size() == 0);
}

TEST_CASE("insert returns the object and the handle")
{
    store<int> s;
    auto h = s.insert(3);
    REQUIRE(get<0>(s[h]) == 3);
    REQUIRE(h.index == 0);
}

TEST_CASE("erase invalidates the handle")
{
    store<int> s;
    auto h1 = s.insert(0);
    auto h2 = s.insert(1);
    s.erase(h1);
    REQUIRE(s.has_handle(h2));
    REQUIRE(!s.has_handle(h1));
    REQUIRE(get<0>(s[h2]) == 1);
    REQUIRE(s.size() == 1);
}

TEST_CASE("iteration")
{
    GIVEN("a store with some values, jumbled due to erasing")
    {
        store<int> s;
        std::vector keys{ s.insert(0) };
        keys.push_back(s.insert(1));
        s.erase(keys[0]);
        keys.push_back(s.insert(2));
        keys.push_back(s.insert(3));
        s.erase(keys[2]);
        keys.push_back(s.insert(4));
        keys.push_back(s.insert(5));
        s.erase(keys[4]);
        keys.push_back(s.insert(6));
        s.erase(keys[6]);
        std::set values{ 1, 3, 5 };
        WHEN("iterating with naked iterator")
        {
            THEN("all values appear")
            {
                for (auto i = s.begin(); i != s.end(); ++i) {
                    auto r = *i;
                    auto [v] = r;
                    auto h = r.handle();
                    auto vi = values.find(v);
                    REQUIRE(vi != values.end());
                    values.erase(vi);
                    REQUIRE(get<0>(s[h]) == v);
                }
                REQUIRE(values.empty());
            }
        }
        AND_WHEN("iterating with range for")
        {
            THEN("all values appear")
            {
                for (auto r : s) {
                    auto [v] = r;
                    auto h = r.handle();
                    auto i = values.find(v);
                    REQUIRE(i != values.end());
                    values.erase(i);
                    REQUIRE(get<0>(s[h]) == v);
                    std::cerr << h.index << ' ' << v << '\n';
                }
                REQUIRE(values.empty());
            }
        }
    }
}

TEST_CASE("indexes aren't reused, their generation shifts")
{
    store<int> s;
    auto hp = s.insert(1);
    s.erase(hp);
    auto hq = s.insert(2);
    REQUIRE(hp != hq);
    REQUIRE(hp.index == hq.index);
    REQUIRE(s.has_handle(hq));
    REQUIRE(!s.has_handle(hp));
}

TEST_CASE("pluralized")
{
    store<int, std::string> s;
    auto h1 = s.insert(3, "foo");
    auto h2 = s.insert(5, "bar");
    REQUIRE(get<0>(s[h1]) == 3);
    REQUIRE(get<0>(s[h2]) == 5);
    REQUIRE(get<1>(s[h1]) == "foo");
    REQUIRE(get<1>(s[h2]) == "bar");
    REQUIRE(s[h1] == std::tuple(3, "foo"));
    REQUIRE(s[h2] == std::tuple(5, "bar"));
    s.erase(h1);
    REQUIRE(get<0>(*s.begin<1>()) == "bar");
    REQUIRE(get<0>(*s.begin<0>()) == 5);
    REQUIRE(get<0>(*select<1>(s).begin()) == "bar");
    REQUIRE(get<0>(*select<0>(s).begin()) == 5);
    REQUIRE(get<0>(*select<std::string>(s).begin()) == "bar");
    REQUIRE(get<0>(*select<int>(s).begin()) == 5);
}

TEST_CASE("erase_if")
{
    store<int, std::string> s;
    s.insert(1, "one");
    s.insert(2, "two");
    s.insert(3, "three");
    s.insert(4, "four");
    s.insert(5, "five");
    s.insert(6, "six");
    {
        auto count = erase_if(s, [](auto&& t) { return (get<0>(t) % 2) == 0; });
        REQUIRE(count == 3);
        std::set<std::tuple<int, std::string>> expected{ { 1, "one" },
                                                         { 3, "three" },
                                                         { 5, "five" } };
        for (auto [num, name] : s) {
            auto i = expected.find({ num, name });
            REQUIRE(i != expected.end());
            expected.erase(i);
        }
        REQUIRE(expected.empty());
    }
    {
        auto count
            = erase_if(select<int>(s), [](auto&& t) { return get<0>(t) > 1; });
        REQUIRE(count == 2);
        REQUIRE(s.size() == 1);
        auto [num, name] = *s.begin();
        REQUIRE(num == 1);
        REQUIRE(name == "one");
    }
}
