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

#ifndef COLUMNIST_TYPE_UTILS_HPP
#define COLUMNIST_TYPE_UTILS_HPP

#include <cstddef>
#include <type_traits>
#include <utility>

namespace columnist {

template <typename T, typename U>
using forwarded_like_t = decltype(std::forward_like<T>(std::declval<U>()));

template <template <typename...> class, typename>
inline constexpr bool is_specialization_v = false;

template <template <typename...> class T, typename... Ts>
inline constexpr bool is_specialization_v<T, T<Ts...>> = true;

template <typename S, template <typename...> class T>
concept specialization_of = is_specialization_v<T, S>;

template <typename T, typename, typename... Ts>
static constexpr size_t type_index = 1 + type_index<T, Ts...>;

template <typename T, typename... Ts>
static constexpr size_t type_index<T, T, Ts...> = 0;

template <typename T, typename... Ts>
inline constexpr bool type_is_one_of = (std::is_same_v<T, Ts> || ...);

template <size_t I, size_t... Is>
inline constexpr bool index_is_one_of = ((I == Is) || ...);

template <size_t I, typename T, typename... Ts>
struct nth_type {
    using type = typename nth_type<I - 1, Ts...>::type;
};

template <typename T, typename... Ts>
struct nth_type<0, T, Ts...> {
    using type = T;
};

template <size_t I, typename... Ts>
using nth_type_t = typename nth_type<I, Ts...>::type;

template <typename T>
concept nothrow_movable = std::is_nothrow_move_constructible_v<T>
                       && std::is_nothrow_move_assignable_v<T>;

} // namespace columnist

#endif // COLUMNIST_TYPE_UTILS_HPP
