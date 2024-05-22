#include <catch2/catch_test_macros.hpp>
#include <store/functional.hpp>

#include <memory>

enum class throwing {
    do_throw,
    no_throw
};

template <typename F, typename... Ts>
static constexpr bool can_invoke(F&&, Ts&&...)
{
    return std::is_invocable_v<F, Ts...>;
}

template <throwing may_throw>
struct callable {
    int& operator()(std::unique_ptr<int>,
                    int) & noexcept(may_throw == throwing::no_throw);
    const int& operator()(std::unique_ptr<int>,
                          int) const& noexcept(may_throw == throwing::no_throw);
    int&& operator()(std::unique_ptr<int>,
                     int) && noexcept(may_throw == throwing::no_throw);
    const int&& operator()(std::unique_ptr<int>,
                           int) const&& noexcept(may_throw
                                                 == throwing::no_throw);
};

TEST_CASE("noexcept forwarding", "[swizzle]")
{
    using store::swizzle;
    auto c = swizzle<0, 1>(callable<throwing::no_throw>{});
    REQUIRE(noexcept(c(nullptr, 1)));
    REQUIRE(noexcept(std::move(c)(nullptr, 1)));
    REQUIRE(noexcept(std::as_const(c)(nullptr, 1)));
    REQUIRE(noexcept(std::move(std::as_const(c))(nullptr, 1)));
    auto ct = swizzle<0, 1>(callable<throwing::do_throw>{});
    REQUIRE(!noexcept(ct(nullptr, 1)));
    REQUIRE(!noexcept(std::move(ct)(nullptr, 1)));
    REQUIRE(!noexcept(std::as_const(ct)(nullptr, 1)));
    REQUIRE(!noexcept(std::move(std::as_const(ct))(nullptr, 1)));
}

TEST_CASE("return type", "[swizzle]")
{
    using store::swizzle;
    auto c = swizzle<0, 1>(callable<throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&&, decltype(std::move(c)(nullptr, 1))>);
    REQUIRE(std::is_same_v<const int&, decltype(std::as_const(c)(nullptr, 1))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(c))(nullptr, 1))>);
    auto ct = swizzle<0, 1>(callable<throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&&, decltype(std::move(ct)(nullptr, 1))>);
    REQUIRE(
        std::is_same_v<const int&, decltype(std::as_const(ct)(nullptr, 1))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(ct))(nullptr, 1))>);
}

TEST_CASE("reorder", "[swizzle]")
{
    using store::swizzle;
    auto c = swizzle<1, 0>(callable<throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(1, nullptr))>);
    REQUIRE(std::is_same_v<int&&, decltype(std::move(c)(1, nullptr))>);
    REQUIRE(std::is_same_v<const int&, decltype(std::as_const(c)(1, nullptr))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(c))(1, nullptr))>);
    auto ct = swizzle<1, 0>(callable<throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(1, nullptr))>);
    REQUIRE(std::is_same_v<int&&, decltype(std::move(ct)(1, nullptr))>);
    REQUIRE(
        std::is_same_v<const int&, decltype(std::as_const(ct)(1, nullptr))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(ct))(1, nullptr))>);
}

TEST_CASE("drop first and last", "[swizzle]")
{
    using store::swizzle;
    constexpr void* n = nullptr;
    auto c = swizzle<2, 1>(callable<throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(n, 1, nullptr, "foo"))>);
    REQUIRE(
        std::is_same_v<int&&, decltype(std::move(c)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<const int&,
                           decltype(std::as_const(c)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(c))(
                               n, 1, nullptr, "foo"))>);
    auto ct = swizzle<2, 1>(callable<throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(n, 1, nullptr, "foo"))>);
    REQUIRE(
        std::is_same_v<int&&, decltype(std::move(ct)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<const int&,
                           decltype(std::as_const(ct)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<const int&&,
                           decltype(std::move(std::as_const(ct))(
                               n, 1, nullptr, "foo"))>);
}

TEST_CASE("arg forwarding", "[swizzle]")
{
    using store::swizzle;
    auto cb = swizzle<0>(std::bind_front(callable<throwing::no_throw>{},
                                         std::make_unique<int>(3)));
    REQUIRE(!can_invoke(cb, 3));
    REQUIRE(can_invoke(std::move(cb), 3));
    REQUIRE(!can_invoke(std::as_const(cb), 3));
    REQUIRE(!can_invoke(std::move(std::as_const(cb)), 3));
}

TEST_CASE("swizzle is constexpr", "[swizzle]")
{
    using store::swizzle;
    constexpr auto f = swizzle<1, 0>([](int a, int b) { return a - b; });
    STATIC_REQUIRE(f(5, 2) == -3);
}

namespace {
using CT = callable<throwing::do_throw>;
using CN = callable<throwing::no_throw>;
using T = std::tuple<std::unique_ptr<int>, int>;
using store::apply;
static_assert(std::is_nothrow_invocable_v<decltype(apply(CN{}))&, T&&>);
static_assert(!std::is_nothrow_invocable_v<decltype(apply(CT{}))&, T&&>);
static_assert(std::is_invocable_v<decltype(apply(CT{}))&, T&&>);

static_assert(std::is_nothrow_invocable_v<decltype(apply(CN{}))&&, T&&>);
static_assert(!std::is_nothrow_invocable_v<decltype(apply(CT{}))&&, T&&>);
static_assert(std::is_invocable_v<decltype(apply(CT{}))&&, T&&>);

static_assert(std::is_nothrow_invocable_v<decltype(apply(CN{})), T&&>);
static_assert(!std::is_nothrow_invocable_v<decltype(apply(CT{})), T&&>);
static_assert(std::is_invocable_v<decltype(apply(CT{})), T&&>);

static_assert(std::is_nothrow_invocable_v<decltype(apply(CN{})) const&, T&&>);
static_assert(!std::is_nothrow_invocable_v<decltype(apply(CT{})) const&, T&&>);
static_assert(std::is_invocable_v<decltype(apply(CT{})) const&, T&&>);

static_assert(std::is_nothrow_invocable_v<decltype(apply(CN{})) const&&, T&&>);
static_assert(!std::is_nothrow_invocable_v<decltype(apply(CT{})) const&&, T&&>);
static_assert(std::is_invocable_v<decltype(apply(CT{})) const&&, T&&>);

static_assert(std::is_nothrow_invocable_v<const decltype(apply(CN{})), T&&>);
static_assert(!std::is_nothrow_invocable_v<const decltype(apply(CT{})), T&&>);
static_assert(std::is_invocable_v<const decltype(apply(CT{})), T&&>);

static_assert(!std::is_invocable_v<decltype(apply(CN{}))&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{}))&&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})), T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})), T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&&, T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&&, T&>);
static_assert(!std::is_invocable_v<const decltype(apply(CN{})), T&>);
static_assert(!std::is_invocable_v<const decltype(apply(CT{})), T&>);

static_assert(!std::is_invocable_v<decltype(apply(CN{}))&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{}))&&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})), const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})), const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&&, const T&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&&, const T&>);
static_assert(!std::is_invocable_v<const decltype(apply(CN{})), const T&>);
static_assert(!std::is_invocable_v<const decltype(apply(CT{})), const T&>);

static_assert(!std::is_invocable_v<decltype(apply(CN{}))&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{}))&&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{}))&&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})), const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})), const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CN{})) const&&, const T&&>);
static_assert(!std::is_invocable_v<decltype(apply(CT{})) const&&, const T&&>);
static_assert(!std::is_invocable_v<const decltype(apply(CN{})), const T&&>);
static_assert(!std::is_invocable_v<const decltype(apply(CT{})), const T&&>);

} // namespace

struct TT : std::tuple<std::unique_ptr<int>, int> {
    using tuple::tuple;
};

TEST_CASE("apply works with types inheriting from tuple", "[apply]")
{
    auto a = apply(callable<throwing::no_throw>{});
    TT tt;
    REQUIRE(std::is_same_v<int&, decltype(a(std::move(tt)))>);
    REQUIRE(
        std::is_same_v<const int&, decltype(std::as_const(a)(std::move(tt)))>);
    REQUIRE(std::is_same_v<int&&, decltype(std::move(a)(std::move(tt)))>);
    REQUIRE(
        std::is_same_v<const int&&,
                       decltype(std::move(std::as_const(a))(std::move(tt)))>);
}

TEST_CASE("apply a swizzled function", "[swizzle][apply]")
{
    using store::swizzle;
    auto f
        = swizzle<1, 0>([](int a, std::unique_ptr<int> b) { return a + *b; });
    auto x = apply(f)(std::tuple{ std::make_unique<int>(3), 5 });
    REQUIRE(x == 8);
}

TEST_CASE("apply is constexpr", "[apply]")
{
    constexpr auto a = apply([](int x, int y) { return x - y; });
    STATIC_REQUIRE(a(std::tuple(5, 2)) == 3);
}
