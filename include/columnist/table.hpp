#ifndef STORE_TABLE_HPP
#define STORE_TABLE_HPP

#include "type_utils.hpp"

#include <cassert>
#include <cstdint>
#include <functional>
#include <ranges>
#include <tuple>
#include <utility>
#include <vector>

namespace columnist {

namespace internal {

template <typename T>
struct type_of {
    using type = T;
};

template <typename T>
using type_of_t = typename type_of<T>::type;

static_assert(std::is_same_v<int, type_of_t<int>>);

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
class table {
    template <typename S, typename Is>
    friend class row;

public:
    template <size_t I>
    using element_type = std::tuple_element_t<I, std::tuple<Ts...>>;
    template <typename T>
    static constexpr size_t type_index = columnist::type_index<T, Ts...>;

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

    class iterator;

    struct sentinel {
        size_t size;
    };

    size_t size() const { return rindex_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id
        requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...);

    void erase(row_id);

    void erase(iterator i);

    bool has_row_id(row_id k) const;

    row<table, std::index_sequence_for<Ts...>> operator[](row_id k)
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    row<const table, std::index_sequence_for<Ts...>> operator[](row_id k) const
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    iterator begin();

    sentinel end();

    template <typename P>
    friend size_t erase_if(table& s, P p)
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

    T t;
};

template <size_t... Is, typename F>
inline constexpr auto select(F f)
{
    return selector<F, Is...>{ f };
};

template <size_t... Ins, typename... Ts, typename Table, size_t... Is>
inline constexpr auto select(const row<Table, std::index_sequence<Is...>>& r)
{
    constexpr auto all_indexes_in_range = ((Ins < sizeof...(Is)) && ...);
    static_assert(
        all_indexes_in_range,
        "Valid indexes are ini the range [0..number of elements in row)");
    if constexpr (all_indexes_in_range) {
        constexpr auto indexes = std::array{ Is... };
        return row<Table, std::index_sequence<indexes[Ins]...>>{ r };
    } else {
        return row<Table, std::index_sequence<>>{};
    }
}

template <typename... Ts, typename Table, size_t... Is>
inline constexpr auto select(const row<Table, std::index_sequence<Is...>>& r)
{
    using TT = std::remove_cvref_t<Table>;
    constexpr bool valid_types
        = (type_is_one_of<Ts, typename TT::template element_type<Is>...>
           && ...);
    static_assert(valid_types,
                  "Valid types are those represented by the row instance");
    if constexpr (valid_types) {
        return select<
            type_index<Ts, typename TT::template element_type<Is>...>...>(r);
    } else {
        return row<Table, std::index_sequence<>>{};
    }
}

template <typename... Ts>
class table<Ts...>::iterator {
    friend class table<Ts...>;

public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = row<table, std::index_sequence_for<Ts...>>;
    using reference = value_type;
    using pointer = void;
    using difference_type = ssize_t;
    bool operator==(const iterator&) const = default;

    bool operator==(sentinel end) const { return idx == end.size; }

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
        return row<table, std::index_sequence_for<Ts...>>(s, idx);
    }

    iterator(){};

private:
    iterator(table<Ts...>* s, size_t idx)
    : s(s), idx(idx)
    {}

    table<Ts...>* s = nullptr;
    size_t idx = 0;
};

template <typename... Ts>
auto table<Ts...>::begin() -> iterator
{
    return { this, 0 };
}

template <typename... Ts>
auto table<Ts...>::end() -> sentinel
{
    return { rindex_.size() };
}

template <typename... Ts>
template <typename... Us>
auto table<Ts...>::insert(Us&&... us) -> row_id
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
void table<Ts...>::erase(row_id k)
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
void table<Ts...>::erase(iterator i)
{
    assert(i.s == this);
    erase(rindex_[i.idx]);
}

template <typename... Ts>
bool table<Ts...>::has_row_id(row_id k) const
{
    if (k.index >= index_.size()) { return false; }
    auto data_idx = index_[k.index].index;
    if (data_idx >= rindex_.size()) { return false; }
    return rindex_[data_idx] == k;
}

} // namespace columnist

template <std::size_t I, typename S, size_t... Is>
struct std::tuple_element<I, columnist::row<S, std::index_sequence<Is...>>> {
    using type = typename S::template element_type<std::tuple_element_t<
        I,
        std::tuple<std::integral_constant<size_t, Is>...>>::value>;
};

template <typename S, size_t... Is>
struct std::tuple_size<columnist::row<S, std::index_sequence<Is...>>>
: std::integral_constant<size_t, sizeof...(Is)> {};
#endif // STORE_TABLE_HPP
