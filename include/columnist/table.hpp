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

#ifndef COLUMNIST_TABLE_HPP
#define COLUMNIST_TABLE_HPP

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

template <typename Table, typename>
class row;

template <typename Table, size_t... Is>
class [[nodiscard]] row<Table, std::index_sequence<Is...>> {
    friend Table;
    template <typename, typename>
    friend class row;

public:
    row() = default;

    template <size_t... PIs>
    explicit row(const row<Table, std::index_sequence<PIs...>>& r) noexcept
    : table_(r.table_), idx_(r.idx_)
    {}

    [[nodiscard]] Table::row_id row_id() const { return table_->rindex_[idx_]; }

    template <size_t I>
        requires(I < sizeof...(Is))
    friend decltype(auto) get(row r)
    {
        using indices = std::tuple<std::integral_constant<size_t, Is>...>;
        constexpr auto column = std::tuple_element_t<I, indices>::value;
        auto& element = std::get<column>(r.table_->data_)[r.idx_];
        if constexpr (std::is_const_v<Table>) {
            return std::as_const(element);
        } else {
            return element;
        }
    }

    template <typename T>
        requires(Table::template has_type<T>)
    friend decltype(auto) get(row r)
    {
        constexpr size_t column = Table::template type_index<T>;
        auto& element = std::get<column>(r.table_->data_)[r.idx_];
        if constexpr (std::is_const_v<Table>) {
            return std::as_const(element);
        } else {
            return element;
        }
    }

    template <typename... Ts>
    bool operator==(const std::tuple<Ts...>& rh) const
    {
        auto elementwise_equality = [this](const auto&... vs) {
            return ((std::get<Is>(table_->data_)[idx_] == vs) && ...);
        };
        return std::apply(elementwise_equality, rh);
    }

    template <typename S>
        requires(std::is_same_v<const S&, const Table&>)
    bool operator==(const row<S, std::index_sequence<Is...>>& rh) const noexcept
    {
        return table_ == rh.table_ && idx_ == rh.idx_;
    }

private:
    row(Table* store, size_t idx)
    : table_(store), idx_(idx)
    {}

    Table* table_ = nullptr;
    size_t idx_ = 0;
};

template <typename>
constexpr bool is_row_v = false;

template <typename Table, size_t... Is>
constexpr bool is_row_v<row<Table, std::index_sequence<Is...>>> = true;

template <typename R>
concept row_range = is_row_v<decltype(*std::declval<R&>().begin())>;

template <typename... Ts>
class table {
    template <typename S, typename Is>
    friend class row;

public:
    template <size_t I>
    using element_type = std::tuple_element_t<I, std::tuple<Ts...>>;
    template <typename T>
    static constexpr bool has_type = columnist::type_is_one_of<T, Ts...>;
    template <typename T>
    static constexpr size_t type_index = columnist::type_index<T, Ts...>;

    using row = columnist::row<table, std::index_sequence_for<Ts...>>;
    using const_row
        = columnist::row<const table, std::index_sequence_for<Ts...>>;

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

    template <typename T>
    class iterator_t;

    using iterator = iterator_t<table>;
    using const_iterator = iterator_t<const table>;

    struct sentinel {
        size_t size;
    };

    size_t size() const { return rindex_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id
        requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...);

    void erase(row_id);

    void erase(const_iterator i);

    bool has_row_id(row_id k) const;

    row operator[](row_id k)
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    const_row operator[](row_id k) const
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    iterator begin();
    const_iterator begin() const;
    const_iterator cbegin() const;
    sentinel end() const;
    sentinel cend() const;

    template <typename Predicate>
    friend size_t erase_if(table& s, Predicate predicate)
    {
        size_t rv = 0;
        auto i = s.begin();
        while (i != s.end()) {
            if (predicate(*i)) {
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

template <typename F, size_t... Is>
struct function_selector {
    template <typename S, typename Idxs>
    decltype(auto) operator()(row<S, Idxs> r)
    {
        auto f = [this, &r]<size_t... I>(std::index_sequence<I...>) {
            using new_idxs = std::index_sequence<std::tuple_element_t<
                Is,
                std::tuple<std::integral_constant<size_t, I>...>>::value...>;
            return captured_function(row<S, new_idxs>(r));
        };
        return f(Idxs{});
    }

    F captured_function;
};

template <size_t... Is, typename F>
    requires(not is_row_v<F> && not row_range<F>)
constexpr auto select(F f)
{
    return function_selector<F, Is...>{ f };
};

template <size_t... Ins, typename Table, size_t... Is>
    requires((Ins < sizeof...(Is)) && ...)
constexpr auto select(const row<Table, std::index_sequence<Is...>>& r)
{
    constexpr auto indexes = std::array{ Is... };
    return row<Table, std::index_sequence<indexes[Ins]...>>{ r };
}

template <typename... Ts, typename Table, size_t... Is>
    requires((type_is_one_of<Ts, typename Table::template element_type<Is>...>
              && ...))
constexpr auto select(const row<Table, std::index_sequence<Is...>>& r)
{
    return select<
        type_index<Ts, typename Table::template element_type<Is>...>...>(r);
}

template <typename F, typename... Ts>
struct [[nodiscard]] function_type_selector {
    template <typename S, typename Idxs>
    decltype(auto) operator()(row<S, Idxs> r)
    {
        return captured_function(select<Ts...>(r));
    }

    F captured_function;
};

template <typename... Ts, typename F>
    requires(not is_row_v<F> && not row_range<F>)
[[nodiscard]] constexpr auto select(F f)
{
    return function_type_selector<F, Ts...>{ f };
}

template <row_range R, size_t... Is>
struct [[nodiscard]] range_index_selector {
    using range_iterator = decltype(std::declval<R&>().begin());

    struct iterator : range_iterator {
        explicit iterator(range_iterator it) noexcept
        : range_iterator(it)
        {}

        auto operator*() const
        {
            const range_iterator& i = *this;
            return select<Is...>(*i);
        }

        using reference = void;
        using pointer = void;
        using difference_type = ssize_t;
        using iterator_category = std::forward_iterator_tag;
        using value_type = decltype(select<Is...>(
            std::declval<typename range_iterator::value_type>()));
    };

    iterator begin() const
    {
        using std::ranges::begin;
        return iterator{ begin(captured_range) };
    }

    iterator cbegin() const { return begin(); }

    auto end() const
    {
        using std::ranges::end;
        return end(captured_range);
    }

    auto cend() const { return end(); }

    R& captured_range;
};

template <size_t... Is, row_range R>
range_index_selector<R, Is...> select(R& r)
    requires requires { select<Is...>(*r.begin()); }
{
    return { r };
}

template <size_t... Is>
struct [[nodiscard]] range_selector_maker {};

template <row_range R, size_t... Is>
range_index_selector<R, Is...> operator|(R& r, range_selector_maker<Is...>)
    requires requires { (select<Is...>(*r.begin())); }
{
    return { r };
};

template <size_t... Is>
range_selector_maker<Is...> select()
{
    return {};
}

template <row_range R, typename... Ts>
struct [[nodiscard]] range_type_selector {
    using range_iterator = decltype(std::declval<R&>().begin());

    struct iterator : range_iterator {
        explicit iterator(range_iterator it) noexcept
        : range_iterator(it)
        {}

        auto operator*() const
        {
            const range_iterator& i = *this;
            return select<Ts...>(*i);
        }

        using reference = void;
        using pointer = void;
        using difference_type = ssize_t;
        using iterator_category = std::forward_iterator_tag;
        using value_type = decltype(select<Ts...>(
            std::declval<typename range_iterator::value_type>()));
    };

    iterator begin() const
    {
        using std::ranges::begin;
        return iterator{ begin(captured_range) };
    }

    iterator cbegin() const { return begin(); }

    auto end() const
    {
        using std::ranges::end;
        return end(captured_range);
    }

    auto cend() const { return end(); }

    R& captured_range;
};

template <typename... Ts, row_range R>
range_type_selector<R, Ts...> select(R& r)
    requires requires { select<Ts...>(*r.begin()); }
{
    return { r };
}

template <typename... Ts>
struct [[nodiscard]] range_type_selector_maker {};

template <row_range R, typename... Ts>
range_type_selector<R, Ts...> operator|(R& r, range_type_selector_maker<Ts...>)
    requires requires { select<Ts...>(*r.begin()); }
{
    return { r };
}

template <typename... Ts>
range_type_selector_maker<Ts...> select()
{
    return {};
}

template <typename... Ts>
template <typename T>
class table<Ts...>::iterator_t {
    friend class table<Ts...>;

public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = columnist::row<T, std::index_sequence_for<Ts...>>;
    using reference = value_type;
    using pointer = void;
    using difference_type = ssize_t;

    iterator_t() = default;

    operator iterator_t<const T>() const noexcept { return { table_, idx_ }; }

    template <typename TT>
        requires(std::is_same_v<const TT, T>)
    iterator_t& operator=(const iterator_t<TT>& tt) noexcept
    {
        table_ = tt.table_;
        idx_ = tt.idx_;
    }

    template <typename TT>
        requires(std::is_same_v<const TT, const T>)
    bool operator==(const iterator_t<TT>& rh) const noexcept
    {
        return table_ == rh.table_ && idx_ == rh.idx_;
    };

    bool operator==(sentinel end) const noexcept { return idx_ == end.size; }

    template <typename Self>
    Self& operator++(this Self& self) noexcept
    {
        ++self.idx_;
        return self;
    }

    template <typename Self>
    Self operator++(this Self& self, int) noexcept
    {
        auto copy = self;
        ++self;
        return copy;
    }

    value_type operator*() const noexcept { return value_type(table_, idx_); }

private:
    iterator_t(T* s, size_t idx)
    : table_(s), idx_(idx)
    {}

    T* table_ = nullptr;
    size_t idx_ = 0;
};

template <typename... Ts>
auto table<Ts...>::begin() -> iterator
{
    return { this, 0 };
}

template <typename... Ts>
auto table<Ts...>::begin() const -> const_iterator
{
    return { this, 0 };
}

template <typename... Ts>
auto table<Ts...>::cbegin() const -> const_iterator
{
    return { this, 0 };
}

template <typename... Ts>
auto table<Ts...>::end() const -> sentinel
{
    return { rindex_.size() };
}

template <typename... Ts>
auto table<Ts...>::cend() const -> sentinel
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
void table<Ts...>::erase(const_iterator i)
{
    assert(i.table_ == this);
    erase(rindex_[i.idx_]);
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
    using basic_type = typename S::template element_type<std::tuple_element_t<
        I,
        std::tuple<std::integral_constant<size_t, Is>...>>::value>;
    using type = std::
        conditional_t<std::is_const_v<S>, const basic_type&, basic_type&>;
};

template <typename S, size_t... Is>
struct std::tuple_size<columnist::row<S, std::index_sequence<Is...>>>
: std::integral_constant<size_t, sizeof...(Is)> {};

#endif // COLUMNIST_TABLE_HPP
