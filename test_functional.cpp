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

template <typename T, throwing may_throw>
struct callable {
    T operator()(std::unique_ptr<int>,
                 int) & noexcept(may_throw == throwing::no_throw);
    T operator()(std::unique_ptr<int>,
                 int) const& noexcept(may_throw == throwing::no_throw);
    T operator()(std::unique_ptr<int>,
                 int) && noexcept(may_throw == throwing::no_throw);
    T operator()(std::unique_ptr<int>,
                 int) const&& noexcept(may_throw == throwing::no_throw);
};

TEST_CASE("noexcept forwarding")
{
    using store::swizzle;
    auto c = swizzle<0, 1>(callable<int&, throwing::no_throw>{});
    REQUIRE(noexcept(c(nullptr, 1)));
    REQUIRE(noexcept(std::move(c)(nullptr, 1)));
    REQUIRE(noexcept(std::as_const(c)(nullptr, 1)));
    REQUIRE(noexcept(std::move(std::as_const(c))(nullptr, 1)));
    auto ct = swizzle<0, 1>(callable<int&, throwing::do_throw>{});
    REQUIRE(!noexcept(ct(nullptr, 1)));
    REQUIRE(!noexcept(std::move(ct)(nullptr, 1)));
    REQUIRE(!noexcept(std::as_const(ct)(nullptr, 1)));
    REQUIRE(!noexcept(std::move(std::as_const(ct))(nullptr, 1)));
}

TEST_CASE("return type")
{
    using store::swizzle;
    auto c = swizzle<0, 1>(callable<int&, throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&, decltype(std::move(c)(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&, decltype(std::as_const(c)(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(c))(nullptr, 1))>);
    auto ct = swizzle<0, 1>(callable<int&, throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&, decltype(std::move(ct)(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&, decltype(std::as_const(ct)(nullptr, 1))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(ct))(nullptr, 1))>);
}

TEST_CASE("reorder")
{
    using store::swizzle;
    auto c = swizzle<1, 0>(callable<int&, throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(1, nullptr))>);
    REQUIRE(std::is_same_v<int&, decltype(std::move(c)(1, nullptr))>);
    REQUIRE(std::is_same_v<int&, decltype(std::as_const(c)(1, nullptr))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(c))(1, nullptr))>);
    auto ct = swizzle<1, 0>(callable<int&, throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(1, nullptr))>);
    REQUIRE(std::is_same_v<int&, decltype(std::move(ct)(1, nullptr))>);
    REQUIRE(std::is_same_v<int&, decltype(std::as_const(ct)(1, nullptr))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(ct))(1, nullptr))>);
}

TEST_CASE("drop first and last")
{
    using store::swizzle;
    constexpr void* n = nullptr;
    auto c = swizzle<2, 1>(callable<int&, throwing::no_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(c(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<int&, decltype(std::move(c)(n, 1, nullptr, "foo"))>);
    REQUIRE(
        std::is_same_v<int&, decltype(std::as_const(c)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(c))(
                               n, 1, nullptr, "foo"))>);
    auto ct = swizzle<2, 1>(callable<int&, throwing::do_throw>{});
    REQUIRE(std::is_same_v<int&, decltype(ct(n, 1, nullptr, "foo"))>);
    REQUIRE(
        std::is_same_v<int&, decltype(std::move(ct)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::as_const(ct)(n, 1, nullptr, "foo"))>);
    REQUIRE(std::is_same_v<int&,
                           decltype(std::move(std::as_const(ct))(
                               n, 1, nullptr, "foo"))>);
}

TEST_CASE("arg forwarding")
{
    using store::swizzle;
    auto cb = swizzle<0>(std::bind_front(callable<int&, throwing::no_throw>{},
                                         std::make_unique<int>(3)));
    REQUIRE(!can_invoke(cb, 3));
    REQUIRE(can_invoke(std::move(cb), 3));
    REQUIRE(!can_invoke(std::as_const(cb), 3));
    REQUIRE(!can_invoke(std::move(std::as_const(cb)), 3));
}
