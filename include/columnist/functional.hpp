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

template <size_t... Ids>
inline constexpr auto swizzle = []<typename F>(F&& f) {
    return [f = std::forward<F>(f)]<typename self, typename... Ts>(
               this self&&,
               Ts&&... ts) noexcept(std::
                                        is_nothrow_invocable_v<
                                            forwarded_like_t<self, F&>,
                                            std::tuple_element_t<
                                                Ids,
                                                std::tuple<Ts...>>...>)
               -> std::invoke_result_t<
                   forwarded_like_t<self, F&>,
                   std::tuple_element_t<Ids, std::tuple<Ts...>>...> {
        auto tup = std::forward_as_tuple(std::forward<Ts>(ts)...);
        return std::forward_like<self>(f)(std::get<Ids>(std::move(tup))...);
    };
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

inline constexpr auto apply = []<typename F>(F&& f) {
    return [f = std::forward<F>(
                f)]<typename Self, appliccable<forwarded_like_t<Self, F>> T>(
               this Self&&, T&& t) noexcept(internal::
                                                is_nothrow_appliccable_v<
                                                    forwarded_like_t<Self, F>,
                                                    T>) -> decltype(auto) {
        auto call
            = [&]<size_t... Is>(std::index_sequence<Is...>) -> decltype(auto) {
            using internal::get;
            return std::forward_like<Self>(f)(get<Is>(std::forward<T>(t))...);
        };
        constexpr auto arity = internal::tuple_size_v<T>;
        return std::invoke(call, std::make_index_sequence<arity>{});
    };
};

} // namespace columnist

#endif // COLUMNIST_FUNCTIONAL_HPP
