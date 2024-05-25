#include <columnist/table.hpp>

#include <catch2/catch_test_macros.hpp>

#include <iostream>
#include <map>
#include <set>
using columnist::table;

using table_ids = columnist::table<int, double, std::string>;

template <typename T, typename U = T, typename = void>
struct is_nothrow_comparable {
    static constexpr bool value = false;
};

template <typename T, typename U>
struct is_nothrow_comparable<
    T,
    U,
    std::void_t<decltype(std::declval<T>() == std::declval<U>())>> {
    static constexpr bool value
        = noexcept(std::declval<T>() == std::declval<U>());
};

template <typename T, typename U = T>
inline constexpr bool is_nothrow_comparable_v
    = is_nothrow_comparable<T, U>::value;

static_assert(std::is_nothrow_constructible_v<table_ids::iterator>);
static_assert(std::is_nothrow_copy_constructible_v<table_ids::iterator>);
static_assert(std::is_nothrow_constructible_v<table_ids::const_iterator>);
static_assert(std::is_nothrow_copy_constructible_v<table_ids::const_iterator>);
static_assert(std::is_nothrow_convertible_v<table_ids::iterator,
                                            table_ids::const_iterator>);

static_assert(not std::is_nothrow_assignable_v<table_ids::iterator&,
                                               table_ids::const_iterator>);
static_assert(std::is_nothrow_assignable_v<table_ids::const_iterator&,
                                           table_ids::iterator>);

static_assert(is_nothrow_comparable_v<table_ids::iterator>);
static_assert(
    is_nothrow_comparable_v<table_ids::iterator, table_ids::const_iterator>);
static_assert(
    is_nothrow_comparable_v<table_ids::const_iterator, table_ids::iterator>);
static_assert(is_nothrow_comparable_v<table_ids::const_iterator>);
static_assert(
    is_nothrow_comparable_v<table_ids::iterator, table_ids::sentinel>);
static_assert(
    is_nothrow_comparable_v<table_ids::const_iterator, table_ids::sentinel>);
static_assert(
    is_nothrow_comparable_v<table_ids::iterator, table_ids::iterator>);

TEST_CASE("a default constructed columnist is empty")
{
    table<int> s;
    REQUIRE(s.empty());
    REQUIRE(s.size() == 0);
}

TEST_CASE("insert returns the object and the row_id")
{
    table<int> s;
    auto h = s.insert(3);
    REQUIRE(get<0>(s[h]) == 3);
    REQUIRE(h.index == 0);
}

TEST_CASE("erase invalidates the row_id")
{
    table<int> s;
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
    GIVEN("a columnist with some values, jumbled due to erasing")
    {
        table<int> s;
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
        AND_WHEN("iterating with mixed const_iterator and iterator")
        {
            THEN("they refer to the same values")
            {
                for (auto [i, ci] = std::tuple(s.begin(), s.cbegin());
                     ci != s.cend();
                     ++i, ++ci) {
                    REQUIRE(*i == *ci);
                    REQUIRE((*i).row_id() == (*ci).row_id());
                }
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
        AND_WHEN("iterating with range for over a const table")
        {
            THEN("all values appear")
            {
                for (auto r : std::as_const(s)) {
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
    table<int> s;
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
    table<int, std::string> s;
    auto h1 = s.insert(3, "foo");
    auto h2 = s.insert(5, "bar");
    REQUIRE(get<0>(s[h1]) == 3);
    REQUIRE(get<0>(s[h2]) == 5);
    REQUIRE(get<1>(s[h1]) == "foo");
    REQUIRE(get<1>(s[h2]) == "bar");
    REQUIRE(s[h1] == std::tuple(3, "foo"));
    REQUIRE(s[h2] == std::tuple(5, "bar"));
    s.erase(h1);
    REQUIRE(get<0>(columnist::select<1>(*s.begin())) == "bar");
    REQUIRE(get<0>(columnist::select<0>(*s.begin())) == 5);
    REQUIRE(get<0>(columnist::select<std::string>(*s.begin())) == "bar");
    REQUIRE(get<0>(columnist::select<int>(*s.begin())) == 5);
}

TEST_CASE("erase_if")
{
    table<int, std::string> s;
    s.insert(1, "one");
    s.insert(2, "two");
    s.insert(3, "three");
    s.insert(4, "four");
    s.insert(5, "five");
    s.insert(6, "six");
    {
        auto count
            = erase_if(s, [](const auto& t) { return (get<0>(t) % 2) == 0; });
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
        auto count = erase_if(s, columnist::select<0>([](const auto& t) {
                                  return get<0>(t) > 1;
                              }));
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
    table<C<0>, C<1>, C<2>, C<3>, C<4>> s;
    s.insert('a', 'b', 'c', 'd', 'e');
    auto r1 = *s.begin();
    auto rcde_num = columnist::select<2, 3, 4>(r1);
    REQUIRE(get<0>(rcde_num) == 'c');
    REQUIRE(get<1>(rcde_num) == 'd');
    REQUIRE(get<2>(rcde_num) == 'e');
    auto rdc_num = columnist::select<1, 0>(rcde_num);
    REQUIRE(get<0>(rdc_num) == 'd');
    REQUIRE(get<1>(rdc_num) == 'c');
    auto rcde = columnist::select<C<2>, C<3>, C<4>>(r1);
    REQUIRE(get<0>(rcde) == 'c');
    REQUIRE(get<1>(rcde) == 'd');
    REQUIRE(get<2>(rcde) == 'e');
    auto rdc = columnist::select<C<3>, C<2>>(rcde);
    REQUIRE(get<0>(rdc) == 'd');
    REQUIRE(get<1>(rdc) == 'c');
    auto race = columnist::select<C<0>, C<2>, C<4>>(r1);
    REQUIRE(get<0>(race) == 'a');
    REQUIRE(get<1>(race) == 'c');
    REQUIRE(get<2>(race) == 'e');
    auto rae = columnist::select<C<0>, C<4>>(race);
    REQUIRE(get<0>(rae) == 'a');
    REQUIRE(get<1>(rae) == 'e');
    auto [a, e] = rae;
    REQUIRE(a == 'a');
    REQUIRE(e == 'e');
}

TEST_CASE("select range")
{
    GIVEN("a range with values of different types")
    {
        table<C<0>, C<1>, C<2>, C<3>, C<4>> s;

        s.insert('a', 'b', 'c', 'd', 'e');
        s.insert('A', 'B', 'C', 'D', 'E');
        using tup = std::tuple<C<0>, C<2>, C<4>>;
        std::array result{ tup{ 'a', 'c', 'e' }, tup{ 'A', 'C', 'E' } };
        WHEN("selecting over indexes on a non-const range")
        {
            THEN("the value are iterated over and can be modified")
            {
                size_t idx = 0;
                for (auto [a, c, e] : s | columnist::select<0, 2, 4>()) {
                    STATIC_REQUIRE(std::is_same_v<C<0>&, decltype(a)>);
                    REQUIRE(a == std::get<C<0>>(result[idx]));
                    REQUIRE(c == std::get<C<2>>(result[idx]));
                    REQUIRE(e == std::get<C<4>>(result[idx]));
                    a.c = a + 1;
                    ++idx;
                }
                REQUIRE(idx == 2);
            }
        }
        AND_WHEN("selecting over indexes on a const range")
        {
            THEN("the value are iterated over and can be modified")
            {
                size_t idx = 0;
                for (auto [a, c, e] :
                     std::as_const(s) | columnist::select<0, 2, 4>()) {
                    STATIC_REQUIRE(std::is_same_v<const C<0>&, decltype(a)>);
                    REQUIRE(a == std::get<C<0>>(result[idx]));
                    REQUIRE(c == std::get<C<2>>(result[idx]));
                    REQUIRE(e == std::get<C<4>>(result[idx]));
                    ++idx;
                }
                REQUIRE(idx == 2);
            }
        }
        AND_WHEN("selecting over types on a non-const range")
        {
            THEN("the value are iterated over and can be modified")
            {
                size_t idx = 0;
                for (auto [a, c, e] :
                     s | columnist::select<C<0>, C<2>, C<4>>()) {
                    STATIC_REQUIRE(std::is_same_v<C<0>&, decltype(a)>);
                    REQUIRE(a == std::get<C<0>>(result[idx]));
                    REQUIRE(c == std::get<C<2>>(result[idx]));
                    REQUIRE(e == std::get<C<4>>(result[idx]));
                    a.c = a + 1;
                    ++idx;
                }
                REQUIRE(idx == 2);
            }
        }
        AND_WHEN("selecting over types on a const range")
        {
            THEN("the value are iterated over and can be modified")
            {
                size_t idx = 0;
                for (auto [a, c, e] :
                     std::as_const(s) | columnist::select<C<0>, C<2>, C<4>>()) {
                    STATIC_REQUIRE(std::is_same_v<const C<0>&, decltype(a)>);
                    REQUIRE(a == std::get<C<0>>(result[idx]));
                    REQUIRE(c == std::get<C<2>>(result[idx]));
                    REQUIRE(e == std::get<C<4>>(result[idx]));
                    ++idx;
                }
                REQUIRE(idx == 2);
            }
        }
    }
}
