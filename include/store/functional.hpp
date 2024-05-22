#ifndef STORE_FUNCTIONAL_HPP
#define STORE_FUNCTIONAL_HPP

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace store {
template <typename F, size_t... Is>
struct swizzle_ {
    template <typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    auto operator()(Ts&&... ts) & noexcept(
        std::is_nothrow_invocable_v<
            F&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<F&,
                                std::tuple_element_t<Is, std::tuple<Ts...>>...>
    {
        return call(f, std::forward<Ts>(ts)...);
    }

    template <typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    auto operator()(Ts&&... ts) && noexcept(
        std::is_nothrow_invocable_v<
            F&&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<F&&,
                                std::tuple_element_t<Is, std::tuple<Ts...>>...>
    {
        return call(std::move(f), std::forward<Ts>(ts)...);
    }

    template <typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    auto operator()(Ts&&... ts) const& noexcept(
        std::is_nothrow_invocable_v<
            const F&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<const F&,
                                std::tuple_element_t<Is, std::tuple<Ts...>>...>
    {
        return call(f, std::forward<Ts>(ts)...);
    }

    template <typename... Ts>
        requires((Is < sizeof...(Ts)) && ...)
    auto operator()(Ts&&... ts) const&& noexcept(
        std::is_nothrow_invocable_v<
            const F&&,
            std::tuple_element_t<Is, std::tuple<Ts...>>...>)
        -> std::invoke_result_t<const F&&,
                                std::tuple_element_t<Is, std::tuple<Ts...>>...>
    {
        return call(std::move(f), std::forward<Ts>(ts)...);
    }

    template <typename FF, typename... Ts>
    static decltype(auto) call(FF&& ff, Ts&&... ts)
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
struct tuple_element {
    using type = std::tuple_element_t<I, T>;
};

template <size_t I, typename T>
struct tuple_element<I, const T> {
    using type = const typename tuple_element<I, T>::type;
};

template <size_t I, typename T>
struct tuple_element<I, T&> {
    using type = typename tuple_element<I, T>::type&;
};

template <size_t I, typename T>
using tuple_element_t = typename tuple_element<I, T>::type;

template <typename... Ts>
std::tuple<Ts...>& get_tuple(std::tuple<Ts...>&);

template <typename... Ts>
std::tuple<Ts...> get_tuple(std::tuple<Ts...>&&);

template <typename... Ts>
const std::tuple<Ts...>& get_tuple(std::tuple<Ts...> const&);

template <typename... Ts>
const std::tuple<Ts...> get_tuple(std::tuple<Ts...> const&&);

template <typename T>
using as_tuple = decltype(get_tuple(std::declval<T>()));
} // namespace internal

template <typename T, typename F>
concept appliccable = std::invoke(
    []<size_t... Is>(std::index_sequence<Is...>) {
        return std::is_invocable_v<
            F,
            internal::tuple_element_t<Is, internal::as_tuple<T>>...>;
    },
    std::make_index_sequence<
        std::tuple_size_v<std::remove_cvref_t<internal::as_tuple<T>>>>{});

template <typename F>
struct apply_ {
    template <appliccable<F&> T>
    constexpr decltype(auto) operator()(T&& t) &
    {
        using TT = internal::as_tuple<T>;
        auto&& tt = static_cast<TT&&>(t);
        return std::apply(f, std::forward<TT>(tt));
    }

    template <appliccable<F&&> T>
    constexpr decltype(auto) operator()(T&& t) &&
    {
        using TT = internal::as_tuple<T>;
        auto&& tt = static_cast<TT&&>(t);
        return std::apply(std::move(f), std::forward<TT>(tt));
    }

    template <appliccable<const F&> T>
    constexpr decltype(auto) operator()(T&& t) const&
    {
        using TT = internal::as_tuple<T>;
        auto&& tt = static_cast<TT&&>(t);
        return std::apply(std::as_const(f), std::forward<TT>(tt));
    }

    template <appliccable<const F&&> T>
    constexpr decltype(auto) operator()(T&& t) const&&
    {
        using TT = internal::as_tuple<T>;
        auto&& tt = static_cast<TT&&>(t);
        return std::apply(std::move(f), std::forward<TT>(tt));
    }

    F f;
};

inline constexpr auto apply
    = []<typename F>(F&& f) { return apply_{ std::forward<F>(f) }; };

} // namespace store
#endif // STORE_FUNCTIONAL_HPP
