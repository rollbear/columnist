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

TEST_CASE("insert returns the object and the row_id")
{
    store<int> s;
    auto h = s.insert(3);
    REQUIRE(get<0>(s[h]) == 3);
    REQUIRE(h.index == 0);
}

TEST_CASE("erase invalidates the row_id")
{
    store<int> s;
    auto h1 = s.insert(0);
    auto h2 = s.insert(1);
    s.erase(h1);
    REQUIRE(s.has_row_id(h2));
    REQUIRE(!s.has_row_id(h1));
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
                    auto h = r.row_id();
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
                    auto h = r.row_id();
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
    REQUIRE(s.has_row_id(hq));
    REQUIRE(!s.has_row_id(hp));
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
    REQUIRE(get<0>(table::select<1>(*s.begin())) == "bar");
    REQUIRE(get<0>(table::select<0>(*s.begin())) == 5);
    REQUIRE(get<0>(table::select<std::string>(*s.begin())) == "bar");
    REQUIRE(get<0>(table::select<int>(*s.begin())) == 5);
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
        auto count = erase_if(
            s, table::select<0>([](auto&& t) { return get<0>(t) > 1; }));
        REQUIRE(count == 2);
        REQUIRE(s.size() == 1);
        auto [num, name] = *s.begin();
        REQUIRE(num == 1);
        REQUIRE(name == "one");
    }
}

template <size_t I>
struct C {
    char c;

    operator char() const { return c; }
};

TEST_CASE("select and subselect")
{
    store<C<0>, C<1>, C<2>, C<3>, C<4>> s;
    s.insert('a', 'b', 'c', 'd', 'e');
    auto r1 = *s.begin();
    auto rcde_num = table::select<2, 3, 4>(r1);
    REQUIRE(get<0>(rcde_num) == 'c');
    REQUIRE(get<1>(rcde_num) == 'd');
    REQUIRE(get<2>(rcde_num) == 'e');
    auto rdc_num = table::select<1, 0>(rcde_num);
    REQUIRE(get<0>(rdc_num) == 'd');
    REQUIRE(get<1>(rdc_num) == 'c');
    auto rcde = table::select<C<2>, C<3>, C<4>>(r1);
    REQUIRE(get<0>(rcde) == 'c');
    REQUIRE(get<1>(rcde) == 'd');
    REQUIRE(get<2>(rcde) == 'e');
    auto rdc = table::select<C<3>, C<2>>(rcde);
    REQUIRE(get<0>(rdc) == 'd');
    REQUIRE(get<1>(rdc) == 'c');
    auto race = table::select<C<0>, C<2>, C<4>>(r1);
    REQUIRE(get<0>(race) == 'a');
    REQUIRE(get<1>(race) == 'c');
    REQUIRE(get<2>(race) == 'e');
    auto rae = table::select<C<0>, C<4>>(race);
    REQUIRE(get<0>(rae) == 'a');
    REQUIRE(get<1>(rae) == 'e');
    auto [a, e] = rae;
    REQUIRE(a == 'a');
    REQUIRE(e == 'e');
}
