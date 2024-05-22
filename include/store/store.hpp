#ifndef STORE_STORE_HPP
#define STORE_STORE_HPP

#include <cassert>
#include <cstdint>
#include <functional>
#include <ranges>
#include <tuple>
#include <utility>
#include <vector>

namespace table {

namespace internal {

template <typename T>
struct type_of {
    using type = T;
};

template <typename T>
using type_of_t = typename type_of<T>::type;

static_assert(std::is_same_v<int, type_of_t<int>>);

template <typename T, typename, typename... Ts>
static constexpr size_t type_index = 1 + type_index<T, Ts...>;

template <typename T, typename... Ts>
static constexpr size_t type_index<T, T, Ts...> = 0;

} // namespace internal

template <typename Store, typename>
class row;

template <typename Store, size_t... Is>
class row<Store, std::index_sequence<Is...>> {
    friend Store;
    template <typename, typename>
    friend class row;

public:
    row() = default;

    template <size_t... PIs>
    row(const row<Store, std::index_sequence<PIs...>>& r)
    : store_(r.store_), idx_(r.idx_)
    {}

    Store::row_id row_id() const { return store_->rindex_[idx_]; }

    template <size_t I>
        requires(I < sizeof...(Is))
    friend decltype(auto) get(row r)
    {
        using indices = std::tuple<std::integral_constant<size_t, Is>...>;
        constexpr auto column = std::tuple_element_t<I, indices>::value;
        auto& element = std::get<column>(r.store_->data_)[r.idx_];
        if constexpr (std::is_const_v<Store>) {
            return std::as_const(element);
        } else {
            return element;
        }
    }

    template <typename... Ts>
    bool operator==(const std::tuple<Ts...>& rh) const
    {
        auto elementwise_equality = [this](const auto&... vs) {
            return ((std::get<Is>(store_->data_)[idx_] == vs) && ...);
        };
        return std::apply(elementwise_equality, rh);
    }

private:
    row(Store* store, size_t idx)
    : store_(store), idx_(idx)
    {}

    Store* store_ = nullptr;
    size_t idx_ = 0;
};

template <typename... Ts>
class store {
    template <typename S, typename Is>
    friend class row;

public:
    template <size_t I>
    using element_type = std::tuple_element_t<I, std::tuple<Ts...>>;
    template <typename T>
    static constexpr size_t type_index = internal::type_index<T, Ts...>;

    struct row_id {
        row_id next_generation() const
        {
            auto copy = *this;
            ++copy.generation;
            return copy;
        }

        constexpr row_id(uint32_t i, uint8_t g = 0)
        : index(i & ((1U << 24) - 1)), generation(g)
        {}

        bool operator==(const row_id&) const = default;

        uint32_t index: 24;
        uint8_t generation;
    };

    template <size_t... Is>
    struct iterator;

    struct sentinel {
        size_t size;
    };

    size_t size() const { return rindex_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id
        requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...);

    void erase(row_id);

    template <size_t... Is>
    void erase(iterator<Is...> i);

    bool has_row_id(row_id k) const;

    row<store, std::index_sequence_for<Ts...>> operator[](row_id k)
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    row<const store, std::index_sequence_for<Ts...>> operator[](row_id k) const
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    template <size_t... Is>
    iterator<Is...> begin();

    iterator<> begin();

    sentinel end();

    template <typename P>
    friend size_t erase_if(store& s, P p)
    {
        size_t rv = 0;
        auto i = s.begin();
        while (i != s.end()) {
            if (p(*i)) {
                ++rv;
                s.erase(i);
            } else {
                ++i;
            }
        }
        return rv;
    }

private:
    std::tuple<std::vector<internal::type_of_t<Ts>>...> data_;
    std::vector<row_id> rindex_;
    std::vector<row_id> index_;
    row_id first_free_ = { 0, 0 };
};

template <typename T, size_t... Is>
struct selector {
    template <typename S, typename Idxs>
    decltype(auto) operator()(row<S, Idxs> r)
    {
        auto f = [this, &r]<size_t... I>(std::index_sequence<I...>) {
            using new_idxs = std::index_sequence<std::tuple_element_t<
                Is,
                std::tuple<std::integral_constant<size_t, I>...>>::value...>;
            return t(row<S, new_idxs>(r));
        };
        return f(Idxs{});
    }

    template <typename TT = T>
    auto begin() -> decltype(std::declval<TT&>().template begin<Is...>())
    {
        return t.template begin<Is...>();
    }

    template <typename TT = T>
    auto end() -> decltype(std::declval<TT&>().template end<Is...>())
    {
        return t.template end<Is...>();
    }

    T t;
};

template <size_t... Is, typename F>
inline constexpr auto select(F f)
{
    return selector<F, Is...>{ f };
};

template <typename... Ts, typename T>
inline constexpr auto select(T&& t)
    requires requires(T& t) { t.template begin<0>(); }
{
    using TT = std::remove_cvref_t<T>;
    return selector<T, TT::template type_index<Ts>...>{ std::forward<T>(t) };
}

template <typename... Ts, typename Table, size_t... Is>
inline constexpr auto select(const row<Table, std::index_sequence<Is...>>& r)
{
    using TT = std::remove_cvref_t<Table>;
    return row<Table, std::index_sequence<TT::template type_index<Ts>...>>{ r };
}

template <typename... Ts>
template <size_t... Idxs>
class store<Ts...>::iterator {
    friend class store<Ts...>;

public:
    using iterator_category = std::forward_iterator_tag;

    bool operator==(const iterator&) const = default;

    bool operator==(sentinel s) const { return idx == s.size; }

    iterator& operator++()
    {
        ++idx;
        return *this;
    }

    iterator operator++(int)
    {
        auto copy = *this;
        ++*this;
        return copy;
    }

    auto operator*() const
    {
        if constexpr (sizeof...(Idxs) == 0) {
            return row<store, std::index_sequence_for<Ts...>>(s, idx);
        } else {
            return row<store, std::index_sequence<Idxs...>>(s, idx);
        }
    }

    iterator(){};

private:
    iterator(store<Ts...>* s, size_t idx)
    : s(s), idx(idx)
    {}

    store<Ts...>* s = nullptr;
    size_t idx = 0;
};

template <typename... Ts>
template <size_t... Is>
auto store<Ts...>::begin() -> iterator<Is...>
{
    return { this, 0 };
}

template <typename... Ts>
auto store<Ts...>::begin() -> iterator<>
{
    return { this, 0 };
}

template <typename... Ts>
auto store<Ts...>::end() -> sentinel
{
    return { rindex_.size() };
}

template <typename... Ts>
template <typename... Us>
auto store<Ts...>::insert(Us&&... us) -> row_id
    requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...)
{
    auto data_idx = static_cast<uint32_t>(rindex_.size());
    auto push
        = [&]<size_t... Is, typename T>(std::index_sequence<Is...>, T&& t) {
              ((void)std::get<Is>(data_).emplace_back(
                   std::get<Is>(std::forward<T>(t))),
               ...);
          };
    std::invoke(push,
                std::index_sequence_for<Ts...>{},
                std::forward_as_tuple(std::forward<Us>(us)...));
    if (first_free_.index == index_.size()) {
        rindex_.push_back(first_free_);
        index_.push_back({ data_idx, 0 });
        first_free_ = row_id(static_cast<uint32_t>(index_.size()));
        return index_.back();
    } else {
        auto index_pos = std::exchange(first_free_, index_[first_free_.index]);
        index_[index_pos.index] = { data_idx, index_pos.generation };
        rindex_.push_back(index_pos);
        return index_pos;
    }
}

template <typename... Ts>
void store<Ts...>::erase(row_id k)
{
    assert(k.index < index_.size());
    auto data_idx = index_[k.index].index;
    assert(data_idx < rindex_.size());
    assert(rindex_[data_idx].index == k.index);
    auto assign_from_last_and_pop_back = [this, data_idx](auto I) {
        std::get<I.value>(data_)[data_idx]
            = std::move(std::get<I.value>(data_).back());
        std::get<I.value>(data_).pop_back();
    };
    auto move_last = [&]<size_t... Is>(std::index_sequence<Is...>) {
        (assign_from_last_and_pop_back(std::integral_constant<size_t, Is>{}),
         ...);
    };
    std::invoke(move_last, std::make_index_sequence<sizeof...(Ts)>{});
    if (data_idx != rindex_.size() - 1) {
        rindex_[data_idx] = rindex_.back();
        index_[rindex_[data_idx].index].index = data_idx & ((1U << 24) - 1);
    }
    rindex_.pop_back();
    index_[k.index] = first_free_;
    first_free_ = k.next_generation();
}

template <typename... Ts>
template <size_t... Is>
void store<Ts...>::erase(iterator<Is...> i)
{
    assert(i.s == this);
    erase(rindex_[i.idx]);
}

template <typename... Ts>
bool store<Ts...>::has_row_id(row_id k) const
{
    if (k.index >= index_.size()) { return false; }
    auto data_idx = index_[k.index].index;
    if (data_idx >= rindex_.size()) { return false; }
    return rindex_[data_idx] == k;
}

} // namespace table

template <std::size_t I, typename S, size_t... Is>
struct std::tuple_element<I, table::row<S, std::index_sequence<Is...>>> {
    using type = typename S::template element_type<std::tuple_element_t<
        I,
        std::tuple<std::integral_constant<size_t, Is>...>>::value>;
};

template <typename S, size_t... Is>
struct std::tuple_size<table::row<S, std::index_sequence<Is...>>>
: std::integral_constant<size_t, sizeof...(Is)> {};
#endif // STORE_STORE_HPP
