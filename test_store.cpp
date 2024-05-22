#include <catch2/catch_test_macros.hpp>
#include <store/store.hpp>

#include <iostream>
#include <set>

TEST_CASE("a default constructed store is empty")
{
    store<int> s;
    REQUIRE(s.empty());
    REQUIRE(s.size() == 0);
}

TEST_CASE("insert returns the object and the key")
{
    store<int> s;
    auto [k, v] = s.insert(3);
    STATIC_REQUIRE(std::is_same_v<int&, decltype(v)>);
    REQUIRE(v == 3);
    STATIC_REQUIRE(std::is_same_v<const store<int>::key&, decltype(k)>);
    REQUIRE(k.index == 0);
}

TEST_CASE("erase invalidates the key")
{
    store<int> s;
    auto [k1, v1] = s.insert(0);
    auto [k2, v2] = s.insert(1);
    s.erase(k1);
    REQUIRE(s.has_key(k2));
    REQUIRE(!s.has_key(k1));
    REQUIRE(s[k2] == 1);
    REQUIRE(s.size() == 1);
}

TEST_CASE("iteration")
{
    store<int> s;
    std::vector keys{ std::get<0>(s.insert(0)) };
    keys.push_back(std::get<0>(s.insert(1)));
    keys.push_back(std::get<0>(s.insert(2)));
    keys.push_back(std::get<0>(s.insert(3)));
    keys.push_back(std::get<0>(s.insert(4)));
    keys.push_back(std::get<0>(s.insert(5)));
    keys.push_back(std::get<0>(s.insert(6)));
    s.erase(keys[0]);
    s.erase(keys[2]);
    s.erase(keys[4]);
    s.erase(keys[6]);
    std::set values{ 1, 3, 5 };
    for (auto [k, v] : s) {
        auto i = values.find(v);
        REQUIRE(i != values.end());
        values.erase(i);
        REQUIRE(s[k] == v);
        std::cerr << k.index << ' ' << v << '\n';
    }
    REQUIRE(values.empty());
}
