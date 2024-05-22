#ifndef STORE_FUNCTIONAL_HPP
#define STORE_FUNCTIONAL_HPP

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace internal {
template <typename T, typename U>
using forwarded_like = decltype(std::forward_like<T>(std::declval<U>()));
}

namespace store {
template <typename F, size_t... Is>
struct swizzle_ {
    template <typename Self, typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    constexpr auto operator()(this Self&& self, Ts&&... ts) noexcept(
        std::is_nothrow_invocable_v<
            F&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<internal::forwarded_like<Self, F&>,
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

template <size_t I, typename T>
using tuple_element_t = ::internal::
    forwarded_like<T, typename std::tuple_element_t<I, std::remove_cvref_t<T>>>;
template <typename T, typename... Ts>
auto get_tuple(const std::tuple<Ts...>&)
    -> ::internal::forwarded_like<T, std::tuple<Ts...>&>;

template <typename T>
using as_tuple = decltype(get_tuple<T>(std::declval<T>()));
} // namespace internal

template <typename T, typename F>
concept appliccable = std::invoke(
    []<size_t... Is>(std::index_sequence<Is...>) {
        using TT = internal::as_tuple<T>;
        return std::is_invocable_v<F, internal::tuple_element_t<Is, TT>...>;
    },
    std::make_index_sequence<
        std::tuple_size_v<std::remove_cvref_t<internal::as_tuple<T>>>>{});

template <typename F>
struct apply_ {
    template <typename Self,
              appliccable<::internal::forwarded_like<Self, F&>> T>
    constexpr decltype(auto) operator()(this Self&& self, T&& t) noexcept(
        noexcept(std::apply(std::forward<Self>(self).f,
                            std::forward<internal::as_tuple<T>>(t))))
    {
        using TT = internal::as_tuple<T>;
        return std::apply(std::forward<Self>(self).f, std::forward<TT>(t));
    }

    F f;
};

inline constexpr auto apply
    = []<typename F>(F&& f) { return apply_{ std::forward<F>(f) }; };

} // namespace store
#endif // STORE_FUNCTIONAL_HPP
