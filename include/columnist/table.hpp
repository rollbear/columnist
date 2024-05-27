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

template <typename Table, typename>
class row;

template <typename Table, size_t... Is>
class [[nodiscard]] row<Table, std::index_sequence<Is...>> {
    friend Table;
    template <typename, typename>
    friend class row;

public:
    static constexpr size_t table_column_index(size_t i)
    {
        constexpr std::array indices{ Is... };
        return indices[i];
    }

    using column_types
        = std::tuple<typename Table::template element_type<Is>...>;
    template <size_t I>
    using column_type
        = forwarded_like_t<Table, std::tuple_element_t<I, column_types>>;

    row() = default;

    template <size_t... PIs>
    explicit row(const row<Table, std::index_sequence<PIs...>>& r) noexcept
    : table_(r.table_), idx_(r.idx_)
    {}

    [[nodiscard]] Table::row_id row_id() const { return table_->rindex_[idx_]; }

    template <size_t I>
        requires(I < sizeof...(Is))
    friend column_type<I>& get(row r)
    {
        constexpr auto column = table_column_index(I);
        return std::get<column>(r.table_->data_)[r.idx_];
    }

    template <typename T>
        requires(Table::template has_type<T>)
    friend auto& get(row r)
    {
        constexpr size_t column = Table::template type_index<T>;
        auto& element = std::get<column>(r.table_->data_)[r.idx_];
        return std::forward_like<Table&>(element);
    }

    template <typename... Ts>
    bool operator==(const std::tuple<Ts...>& rh) const
    {
        auto elementwise_equality = [this](const auto&... vs) {
            return ((std::get<Is>(table_->data_)[idx_] == vs) && ...);
        };
        return std::apply(elementwise_equality, rh);
    }

    template <typename T2>
        requires(std::is_same_v<const T2&, const Table&>)
    bool
    operator==(const row<T2, std::index_sequence<Is...>>& rh) const noexcept
    {
        return table_ == rh.table_ && idx_ == rh.idx_;
    }

private:
    row(Table* table, size_t idx)
    : table_(table), idx_(idx)
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

    using indexes = std::index_sequence_for<Ts...>;

public:
    template <size_t I>
    using element_type = std::tuple_element_t<I, std::tuple<Ts...>>;
    template <typename T>
    static constexpr bool has_type = columnist::type_is_one_of<T, Ts...>;
    template <typename T>
    static constexpr size_t type_index = columnist::type_index<T, Ts...>;

    using row = columnist::row<table, indexes>;
    using const_row = columnist::row<const table, indexes>;

    struct row_id {
        row_id next_generation() const
        {
            auto copy = *this;
            ++copy.generation;
            return copy;
        }

        explicit constexpr row_id(uint32_t i, uint8_t g = 0)
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

    struct sentinel {};

    [[nodiscard]] size_t size() const { return rindex_.size(); }

    [[nodiscard]] bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id
        requires(std::is_constructible_v<Ts, Us> && ...);

    void erase(row_id);

    void erase(const_iterator i);

    [[nodiscard]] bool has_row_id(row_id k) const;

    [[nodiscard]] row operator[](row_id k)
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    [[nodiscard]] const_row operator[](row_id k) const
    {
        assert(has_row_id(k));
        return { this, index_[k.index].index };
    }

    [[nodiscard]] iterator begin();
    [[nodiscard]] const_iterator begin() const;
    [[nodiscard]] const_iterator cbegin() const;
    [[nodiscard]] sentinel end() const;
    [[nodiscard]] sentinel cend() const;

    template <typename Predicate>
    friend size_t erase_if(table& t, Predicate predicate)
    {
        size_t rv = 0;
        auto i = t.begin();
        while (i != t.end()) {
            if (predicate(*i)) {
                ++rv;
                t.erase(i);
            } else {
                ++i;
            }
        }
        return rv;
    }

private:
    std::tuple<std::vector<Ts>...> data_;
    std::vector<row_id> rindex_;
    std::vector<row_id> index_;
    row_id first_free_ = row_id{ 0, 0 };
};

template <typename F, size_t... Is>
struct [[nodiscard]] function_selector {
    template <typename Table, typename Idxs>
    decltype(auto) operator()(row<Table, Idxs> r)
    {
        using R = row<Table, Idxs>;
        using new_idxs = std::index_sequence<R::table_column_index(Is)...>;
        return captured_function(row<Table, new_idxs>(r));
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
[[nodiscard]] constexpr auto
select(const row<Table, std::index_sequence<Is...>>& r)
{
    constexpr auto indexes = std::array{ Is... };
    return row<Table, std::index_sequence<indexes[Ins]...>>{ r };
}

template <typename... Ts, typename Table, size_t... Is>
    requires((type_is_one_of<Ts, typename Table::template element_type<Is>...>
              && ...))
[[nodiscard]] constexpr auto
select(const row<Table, std::index_sequence<Is...>>& r)
{
    return select<
        type_index<Ts, typename Table::template element_type<Is>...>...>(r);
}

template <typename F, typename... Ts>
struct [[nodiscard]] function_type_selector {
    template <typename row>
        requires(is_row_v<row>)
    decltype(auto) operator()(row r)
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

    [[nodiscard]] iterator begin() const
    {
        using std::ranges::begin;
        return iterator{ begin(captured_range) };
    }

    [[nodiscard]] iterator cbegin() const { return begin(); }

    [[nodiscard]] auto end() const
    {
        using std::ranges::end;
        return end(captured_range);
    }

    [[nodiscard]] auto cend() const { return end(); }

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

    [[nodiscard]] iterator begin() const
    {
        using std::ranges::begin;
        return iterator{ begin(captured_range) };
    }

    [[nodiscard]] iterator cbegin() const { return begin(); }

    [[nodiscard]] auto end() const
    {
        using std::ranges::end;
        return end(captured_range);
    }

    [[nodiscard]] auto cend() const { return end(); }

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
    using value_type = columnist::row<T, typename T::indexes>;
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

    bool operator==(sentinel) const noexcept { return idx_ == table_->size(); }

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

    [[nodiscard]] value_type operator*() const noexcept
    {
        return value_type(table_, idx_);
    }

private:
    iterator_t(T* table, size_t idx)
    : table_(table), idx_(idx)
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
    return {};
}

template <typename... Ts>
auto table<Ts...>::cend() const -> sentinel
{
    return {};
}

template <typename... Ts>
template <typename... Us>
auto table<Ts...>::insert(Us&&... us) -> row_id
    requires(std::is_constructible_v<Ts, Us> && ...)
{
    auto data_idx = static_cast<uint32_t>(rindex_.size());
    auto push
        = [&]<size_t... Is, typename T>(std::index_sequence<Is...>, T&& t) {
              ((void)std::get<Is>(data_).emplace_back(
                   std::get<Is>(std::forward<T>(t))),
               ...);
          };
    std::invoke(
        push, indexes{}, std::forward_as_tuple(std::forward<Us>(us)...));
    if (first_free_.index == index_.size()) {
        rindex_.push_back(first_free_);
        index_.push_back(row_id{ data_idx, 0 });
        first_free_ = row_id(static_cast<uint32_t>(index_.size()));
        return index_.back();
    } else {
        auto index_pos = std::exchange(first_free_, index_[first_free_.index]);
        index_[index_pos.index] = row_id{ data_idx, index_pos.generation };
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
    std::invoke(move_last, indexes{});
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

template <std::size_t I, typename Table, typename Indexes>
struct std::tuple_element<I, columnist::row<Table, Indexes>> {
    using type =
        typename columnist::row<Table, Indexes>::template column_type<I>&;
};

template <typename Table, size_t... Is>
struct std::tuple_size<columnist::row<Table, std::index_sequence<Is...>>>
: std::integral_constant<size_t, sizeof...(Is)> {};

#endif // COLUMNIST_TABLE_HPP
