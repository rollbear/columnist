#include <catch2/catch_test_macros.hpp>
#include <store/store.hpp>

#include <iostream>
#include <set>

using table::store;

TEST_CASE("a default constructed store is empty")
{
    store<int> s;
    REQUIRE(s.empty());
    REQUIRE(s.size() == 0);
}

TEST_CASE("insert returns the object and the key")
{
    store<int> s;
    auto i = s.insert(3);
    auto k = std::get<0>(*i);
    REQUIRE(std::get<0>(s[k]) == 3);
    REQUIRE(k.index == 0);
}

TEST_CASE("erase invalidates the key")
{
    store<int> s;
    auto i1 = s.insert(0);
    auto k1 = std::get<0>(*i1);
    auto i2 = s.insert(1);
    auto k2 = std::get<0>(*i2);
    s.erase(k1);
    REQUIRE(s.has_key(k2));
    REQUIRE(!s.has_key(k1));
    REQUIRE(std::get<0>(s[k2]) == 1);
    REQUIRE(s.size() == 1);
}

TEST_CASE("iteration")
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
    for (auto [k, v] : s) {
        auto i = values.find(v);
        REQUIRE(i != values.end());
        values.erase(i);
        REQUIRE(std::get<0>(s[k]) == v);
        std::cerr << k.index << ' ' << v << '\n';
    }
    REQUIRE(values.empty());
}

TEST_CASE("indexes aren't reused, their generation shifts")
{
    store<int> s;
    auto p = s.insert(1);
    auto kp = std::get<0>(*p);
    s.erase(p);
    auto q = s.insert(2);
    auto kq = std::get<0>(*q);
    REQUIRE(kp != kq);
    REQUIRE(kp.index == kq.index);
    REQUIRE(s.has_key(kq));
    REQUIRE(!s.has_key(kp));
}

TEST_CASE("pluralized")
{
    store<int, std::string> s;
    auto i1 = s.insert(3, "foo");
    auto i2 = s.insert(5, "bar");
    REQUIRE(std::get<1>(*i1) == 3);
    REQUIRE(std::get<1>(*i2) == 5);
    REQUIRE(std::get<2>(*i1) == "foo");
    REQUIRE(std::get<2>(*i2) == "bar");
    auto k1 = std::get<0>(*i1);
    auto k2 = std::get<0>(*i2);
    REQUIRE(s[k1] == std::tuple(3, "foo"));
    REQUIRE(s[k2] == std::tuple(5, "bar"));
    s.erase(k1);
    REQUIRE(std::get<1>(*s.begin<1>()) == "bar");
    REQUIRE(std::get<1>(*s.begin<0>()) == 5);
    REQUIRE(std::get<1>(*select<1>(s).begin()) == "bar");
    REQUIRE(std::get<1>(*select<0>(s).begin()) == 5);
    REQUIRE(std::get<1>(*select<std::string>(s).begin()) == "bar");
    REQUIRE(std::get<1>(*select<int>(s).begin()) == 5);
}
