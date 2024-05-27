/*
 * columnist C++23 ECS (Entity Component System) library
 *
 * Copyright (C) Björn Fahller
 *
 *  Use, modification and distribution is subject to the
 *  Boost Software License, Version 1.0. (See accompanying
 *  file LICENSE_1_0.txt or copy at
 *  http://www.boost.org/LICENSE_1_0.txt)
 *
 * Project home: https://github.com/rollbear/columnist
 */

#ifndef COLUMNIST_FUNCTIONAL_HPP
#define COLUMNIST_FUNCTIONAL_HPP

#include "type_utils.hpp"

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace columnist {

template <typename F, size_t... Is>
struct swizzle_ {
    template <typename Self, typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    constexpr auto operator()(this Self&& self, Ts&&... ts) noexcept(
        std::is_nothrow_invocable_v<
            F&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<forwarded_like_t<Self, F&>,
                                std::tuple_element_t<Is, std::tuple<Ts...>>...>
    {
        return call(std::forward<Self>(self).f, std::forward<Ts>(ts)...);
    }

    template <typename FF, typename... Ts>
    static constexpr decltype(auto) call(FF&& ff, Ts&&... ts)
    {
        auto tup = std::forward_as_tuple(std::forward<Ts>(ts)...);
        return std::invoke(std::forward<FF>(ff),
                           std::get<Is>(std::move(tup))...);
    }

    F f;
};

template <size_t... Ids>
inline constexpr auto swizzle = []<typename F>(F&& f) {
    return swizzle_<std::remove_cvref_t<F>, Ids...>{ std::forward<F>(f) };
};

namespace internal {

template <typename... Ts>
constexpr std::tuple<Ts...>& as_tupleish(std::tuple<Ts...>& t)
{
    return t;
}

template <typename... Ts>
constexpr const std::tuple<Ts...>& as_tupleish(const std::tuple<Ts...>& t)
{
    return t;
}

template <typename T, size_t N>
constexpr std::array<T, N>& as_tupleish(std::array<T, N>& a)
{
    return a;
}

template <typename T, size_t N>
constexpr const std::array<T, N>& as_tupleish(const std::array<T, N>& a)
{
    return a;
}

template <typename A, typename B>
constexpr std::pair<A, B>& as_tupleish(std::pair<A, B>& p)
{
    return p;
}

template <typename A, typename B>
constexpr const std::pair<A, B>& as_tupleish(const std::pair<A, B>& p)
{
    return p;
}

template <typename T>
concept gettable = requires { std::tuple_size<T>{}; };
template <typename T>
concept tupleish = (gettable<T> || requires(T t) { as_tupleish(t); });

template <size_t I>
inline constexpr auto get = []<typename T>(T&& t) -> decltype(auto) {
    if constexpr (requires { as_tupleish(t); }) {
        auto& tt = as_tupleish(t);
        return std::get<I>(std::forward_like<T>(tt));
    } else {
        if constexpr (requires { std::forward<T>(t).template get<I>(); }) {
            return std::forward<T>(t).template get<I>();
        } else {
            using std::get;
            return get<I>(std::forward<T>(t));
        }
    }
};

template <size_t I, typename T>
using tuple_element_t = decltype(internal::get<I>(std::declval<T>()));

template <typename T, typename... Ts>
auto get_tuple(const std::tuple<Ts...>&)
    -> forwarded_like_t<T, std::tuple<Ts...>>;

template <typename T>
using as_tuple = decltype(get_tuple<T>(std::declval<T>()));

template <typename T>
inline constexpr size_t tuple_size_v = [] {
    using TT = std::remove_cvref_t<T>;
    if constexpr (requires(TT& tt) { get_tuple<TT>(tt); }) {
        return std::tuple_size_v<std::remove_reference_t<as_tuple<TT>>>;
    } else {
        return std::tuple_size_v<TT>;
    }
}();

template <typename F, typename T>
inline constexpr bool is_nothrow_appliccable_v = std::invoke(
    []<size_t... Is>(std::index_sequence<Is...>) {
        using internal::tuple_element_t;
        return std::is_nothrow_invocable_v<F, tuple_element_t<Is, T>...>;
    },
    std::make_index_sequence<internal::tuple_size_v<T>>{});

} // namespace internal

template <typename T, typename F>
concept appliccable = std::invoke(
    []<size_t... Is>(std::index_sequence<Is...>) {
        return std::is_invocable_v<F, internal::tuple_element_t<Is, T>...>;
    },
    std::make_index_sequence<internal::tuple_size_v<T>>{});

template <typename F>
struct [[nodiscard]] apply_ {
    template <typename Self, appliccable<forwarded_like_t<Self, F>> T>
    constexpr decltype(auto) operator()(this Self&& self, T&& t) noexcept(
        internal::is_nothrow_appliccable_v<forwarded_like_t<Self, F>, T>)
    {
        constexpr auto arity = internal::tuple_size_v<T>;
        auto call
            = [&]<size_t... Is>(std::index_sequence<Is...>) -> decltype(auto) {
            using internal::get;
            return std::forward<Self>(self).f(get<Is>(std::forward<T>(t))...);
        };
        return std::invoke(call, std::make_index_sequence<arity>{});
    }

    F f;
};

inline constexpr auto apply
    = []<typename F>(F&& f) { return apply_{ std::forward<F>(f) }; };

} // namespace columnist

#endif // COLUMNIST_FUNCTIONAL_HPP
