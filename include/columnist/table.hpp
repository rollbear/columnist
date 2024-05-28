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

template <typename Table, size_t... table_column_numbers>
class [[nodiscard]] row<Table, std::index_sequence<table_column_numbers...>> {
    friend Table;
    template <typename, typename>
    friend class row;

public:
    static constexpr size_t table_column_number(size_t i)
    {
        constexpr std::array columns{ table_column_numbers... };
        return columns[i];
    }

    using column_types = std::tuple<
        typename Table::template column_type<table_column_numbers>...>;
    template <size_t column_number>
    using column_type
        = forwarded_like_t<Table,
                           std::tuple_element_t<column_number, column_types>>;

    row() = default;

    template <size_t... column_numbers>
    explicit row(
        const row<Table, std::index_sequence<column_numbers...>>& r) noexcept
    : table_(r.table_), offset_(r.offset_)
    {}

    [[nodiscard]] Table::row_id row_id() const
    {
        return table_->rindex_[offset_];
    }

    template <size_t column_number>
        requires(column_number < sizeof...(table_column_numbers))
    friend column_type<column_number>& get(row r)
    {
        constexpr auto table_column = table_column_number(column_number);
        return std::get<table_column>(r.table_->data_)[r.offset_];
    }

    template <typename column_type>
        requires(Table::template has_type<column_type>)
    friend auto& get(row r)
    {
        constexpr size_t table_column
            = Table::template column_type_number<column_type>;
        auto& element = std::get<table_column>(r.table_->data_)[r.offset_];
        return std::forward_like<Table&>(element);
    }

    template <typename... column_numbers>
    bool operator==(const std::tuple<column_numbers...>& rh) const
    {
        auto elementwise_equality = [this](const auto&... vs) {
            return (
                (std::get<table_column_numbers>(table_->data_)[offset_] == vs)
                && ...);
        };
        return std::apply(elementwise_equality, rh);
    }

    template <typename T2>
        requires(std::is_same_v<const T2&, const Table&>)
    bool
    operator==(const row<T2, std::index_sequence<table_column_numbers...>>& rh)
        const noexcept
    {
        return table_ == rh.table_ && offset_ == rh.offset_;
    }

private:
    row(Table* table, size_t offset)
    : table_(table), offset_(offset)
    {}

    Table* table_ = nullptr;
    size_t offset_ = 0;
};

template <typename>
constexpr bool is_row_v = false;

template <typename Table, size_t... column_numbers>
constexpr bool is_row_v<row<Table, std::index_sequence<column_numbers...>>>
    = true;

template <typename R>
concept row_range = is_row_v<decltype(*std::declval<R&>().begin())>;

template <typename... column_types>
class table {
    template <typename, typename>
    friend class row;

    using column_numbers = std::index_sequence_for<column_types...>;

public:
    template <size_t column_number>
    using column_type
        = std::tuple_element_t<column_number, std::tuple<column_types...>>;
    template <typename T>
    static constexpr bool has_type
        = columnist::type_is_one_of<T, column_types...>;
    template <typename column_type>
    static constexpr size_t column_type_number
        = columnist::type_index<column_type, column_types...>;

    using row = columnist::row<table, column_numbers>;
    using const_row = columnist::row<const table, column_numbers>;

    struct row_id {
        row_id next_generation() const
        {
            auto copy = *this;
            ++copy.generation;
            return copy;
        }

        explicit constexpr row_id(uint32_t i, uint8_t g = 0)
        : offset(i & ((1U << 24) - 1)), generation(g)
        {}

        bool operator==(const row_id&) const = default;

        uint32_t offset: 24;
        uint8_t generation;
    };

    template <typename>
    class iterator_t;

    using iterator = iterator_t<table>;
    using const_iterator = iterator_t<const table>;

    struct sentinel {};

    [[nodiscard]] size_t size() const { return rindex_.size(); }

    [[nodiscard]] bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id
        requires(std::is_constructible_v<column_types, Us> && ...);

    void erase(row_id);

    void erase(const_iterator i);

    [[nodiscard]] bool has_row_id(row_id k) const;

    [[nodiscard]] row operator[](row_id k)
    {
        assert(has_row_id(k));
        return { this, index_[k.offset].offset };
    }

    [[nodiscard]] const_row operator[](row_id k) const
    {
        assert(has_row_id(k));
        return { this, index_[k.offset].offset };
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
    std::tuple<std::vector<column_types>...> data_;
    std::vector<row_id> rindex_;
    std::vector<row_id> index_;
    row_id first_free_ = row_id{ 0, 0 };
};

template <typename F, size_t... selected_column_numbers>
struct [[nodiscard]] function_selector {
    template <typename Table, typename columns>
    decltype(auto) operator()(row<Table, columns> r)
    {
        using R = row<Table, columns>;
        using new_columns = std::index_sequence<R::table_column_number(
            selected_column_numbers)...>;
        return captured_function(row<Table, new_columns>(r));
    }

    F captured_function;
};

template <size_t... selected_column_numbers, typename F>
    requires(not is_row_v<F> && not row_range<F>)
constexpr auto select(F f)
{
    return function_selector<F, selected_column_numbers...>{ f };
};

template <size_t... selected_columns, typename Table, size_t... row_columns>
    requires((selected_columns < sizeof...(row_columns)) && ...)
[[nodiscard]] constexpr auto
select(const row<Table, std::index_sequence<row_columns...>>& r)
{
    constexpr auto indexes = std::array{ row_columns... };
    return row<Table, std::index_sequence<indexes[selected_columns]...>>{ r };
}

template <typename... selected_types, typename Table, size_t... column_numbers>
    requires(
        (type_is_one_of<selected_types,
                        typename Table::template column_type<column_numbers>...>
         && ...))
[[nodiscard]] constexpr auto
select(const row<Table, std::index_sequence<column_numbers...>>& r)
{
    return select<
        type_index<selected_types,
                   typename Table::template column_type<column_numbers>...>...>(
        r);
}

template <typename function, typename... selected_types>
struct [[nodiscard]] function_type_selector {
    template <typename row>
        requires(is_row_v<row>)
    decltype(auto) operator()(row r)
    {
        return captured_function(select<selected_types...>(r));
    }

    function captured_function;
};

template <typename... selected_types, typename function>
    requires(not is_row_v<function> && not row_range<function>)
[[nodiscard]] constexpr auto select(function f)
{
    return function_type_selector<function, selected_types...>{ f };
}

template <row_range R, size_t... selected_columns>
struct [[nodiscard]] range_index_selector {
    using underlying_iterator = decltype(std::declval<R&>().begin());

    struct iterator : underlying_iterator {
        explicit iterator(underlying_iterator it) noexcept
        : underlying_iterator(it)
        {}

        auto operator*() const
        {
            const underlying_iterator& i = *this;
            return select<selected_columns...>(*i);
        }

        using difference_type = ssize_t;
        using iterator_category = std::forward_iterator_tag;
        using value_type = decltype(select<selected_columns...>(
            std::declval<typename underlying_iterator::value_type>()));
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

template <typename R, size_t... columns>
concept row_range_with_column_numbers
    = requires(R& r) { select<columns...>(*r.begin()); };

template <size_t... columns, row_range_with_column_numbers<columns...> R>
range_index_selector<R, columns...> select(R& r)
{
    return { r };
}

template <size_t...>
struct [[nodiscard]] range_selector_maker {};

template <size_t... column_numbers,
          row_range_with_column_numbers<column_numbers...> R>
range_index_selector<R, column_numbers...>
operator|(R& r, range_selector_maker<column_numbers...>)
{
    return { r };
};

template <size_t... column_numbers>
range_selector_maker<column_numbers...> select()
{
    return {};
}

template <row_range R, typename... column_types>
struct [[nodiscard]] range_type_selector {
    using underlying_iterator = decltype(std::declval<R&>().begin());

    struct iterator : underlying_iterator {
        explicit iterator(underlying_iterator it) noexcept
        : underlying_iterator(it)
        {}

        auto operator*() const
        {
            const underlying_iterator& i = *this;
            return select<column_types...>(*i);
        }

        using difference_type = ssize_t;
        using iterator_category = std::forward_iterator_tag;
        using value_type = decltype(select<column_types...>(
            std::declval<typename underlying_iterator::value_type>()));
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

template <typename R, typename... column_types>
concept row_range_with_column_types
    = requires(R& r) { select<column_types...>(*r.begin()); };

template <typename... column_types,
          row_range_with_column_types<column_types...> R>
range_type_selector<R, column_types...> select(R& r)
{
    return { r };
}

template <typename...>
struct [[nodiscard]] range_type_selector_maker {};

template <typename... column_types,
          row_range_with_column_types<column_types...> R>
range_type_selector<R, column_types...>
operator|(R& r, range_type_selector_maker<column_types...>)
{
    return { r };
}

template <typename... column_types>
range_type_selector_maker<column_types...> select()
{
    return {};
}

template <typename... column_types>
template <typename T>
class table<column_types...>::iterator_t {
    friend class table<column_types...>;

public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = columnist::row<T, typename T::column_numbers>;
    using difference_type = ssize_t;

    iterator_t() = default;

    operator iterator_t<const T>() const noexcept
    {
        return { table_, offset_ };
    }

    template <typename TT>
        requires(std::is_same_v<const TT, T>)
    iterator_t& operator=(const iterator_t<TT>& tt) noexcept
    {
        table_ = tt.table_;
        offset_ = tt.offset_;
    }

    template <typename TT>
        requires(std::is_same_v<const TT, const T>)
    bool operator==(const iterator_t<TT>& rh) const noexcept
    {
        return table_ == rh.table_ && offset_ == rh.offset_;
    };

    bool operator==(sentinel) const noexcept
    {
        return offset_ == table_->size();
    }

    template <typename Self>
    Self& operator++(this Self& self) noexcept
    {
        ++self.offset_;
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
        return value_type(table_, offset_);
    }

private:
    iterator_t(T* table, size_t offset)
    : table_(table), offset_(offset)
    {}

    T* table_ = nullptr;
    size_t offset_ = 0;
};

template <typename... column_types>
auto table<column_types...>::begin() -> iterator
{
    return { this, 0 };
}

template <typename... column_types>
auto table<column_types...>::begin() const -> const_iterator
{
    return { this, 0 };
}

template <typename... column_types>
auto table<column_types...>::cbegin() const -> const_iterator
{
    return { this, 0 };
}

template <typename... column_types>
auto table<column_types...>::end() const -> sentinel
{
    return {};
}

template <typename... column_types>
auto table<column_types...>::cend() const -> sentinel
{
    return {};
}

template <typename... column_types>
template <typename... Us>
auto table<column_types...>::insert(Us&&... us) -> row_id
    requires(std::is_constructible_v<column_types, Us> && ...)
{
    auto data_idx = static_cast<uint32_t>(rindex_.size());
    auto push
        = [&]<size_t... Is, typename T>(std::index_sequence<Is...>, T&& t) {
              ((void)std::get<Is>(data_).emplace_back(
                   std::get<Is>(std::forward<T>(t))),
               ...);
          };
    std::invoke(
        push, column_numbers{}, std::forward_as_tuple(std::forward<Us>(us)...));
    if (first_free_.offset == index_.size()) {
        rindex_.push_back(first_free_);
        index_.push_back(row_id{ data_idx, 0 });
        first_free_ = row_id(static_cast<uint32_t>(index_.size()));
        return index_.back();
    } else {
        auto index_pos = std::exchange(first_free_, index_[first_free_.offset]);
        index_[index_pos.offset] = row_id{ data_idx, index_pos.generation };
        rindex_.push_back(index_pos);
        return index_pos;
    }
}

template <typename... column_types>
void table<column_types...>::erase(row_id k)
{
    assert(k.offset < index_.size());
    auto data_offset = index_[k.offset].offset;
    assert(data_offset < rindex_.size());
    assert(rindex_[data_offset].offset == k.offset);
    auto assign_from_last_and_pop_back = [this, data_offset](auto I) {
        std::get<I.value>(data_)[data_offset]
            = std::move(std::get<I.value>(data_).back());
        std::get<I.value>(data_).pop_back();
    };
    auto move_last = [&]<size_t... columns>(std::index_sequence<columns...>) {
        (assign_from_last_and_pop_back(
             std::integral_constant<size_t, columns>{}),
         ...);
    };
    std::invoke(move_last, column_numbers{});
    if (data_offset != rindex_.size() - 1) {
        rindex_[data_offset] = rindex_.back();
        index_[rindex_[data_offset].offset].offset
            = data_offset & ((1U << 24) - 1);
    }
    rindex_.pop_back();
    index_[k.offset] = first_free_;
    first_free_ = k.next_generation();
}

template <typename... column_types>
void table<column_types...>::erase(const_iterator i)
{
    assert(i.table_ == this);
    erase(rindex_[i.offset_]);
}

template <typename... column_types>
bool table<column_types...>::has_row_id(row_id k) const
{
    if (k.offset >= index_.size()) { return false; }
    auto data_offset = index_[k.offset].offset;
    if (data_offset >= rindex_.size()) { return false; }
    return rindex_[data_offset] == k;
}

} // namespace columnist

template <std::size_t I, typename Table, typename column_numbers>
struct std::tuple_element<I, columnist::row<Table, column_numbers>> {
    using type =
        typename columnist::row<Table,
                                column_numbers>::template column_type<I>&;
};

template <typename Table, size_t... column_numbers>
struct std::tuple_size<
    columnist::row<Table, std::index_sequence<column_numbers...>>>
: std::integral_constant<size_t, sizeof...(column_numbers)> {};

#endif // COLUMNIST_TABLE_HPP
