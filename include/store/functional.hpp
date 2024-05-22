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

} // namespace store
#endif // STORE_FUNCTIONAL_HPP
