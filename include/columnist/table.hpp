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

template <typename, typename>
class row;

template <typename table_type, size_t... table_column_numbers>
class [[nodiscard]] row<table_type,
                        std::index_sequence<table_column_numbers...>> {
    friend table_type;
    template <typename, typename>
    friend class row;

public:
    static constexpr size_t table_column_number(size_t i);

    template <typename T>
    static constexpr bool has_type = type_is_one_of<
        T,
        typename table_type::template column_type<table_column_numbers>...>;

    template <size_t... column_numbers, typename T2>
        requires std::is_assignable_v<table_type*&, T2*>
                  && (index_is_one_of<table_column_numbers, column_numbers...>
                      && ...)
    explicit row(
        const row<T2, std::index_sequence<column_numbers...>>& r) noexcept
    : table_(r.table_), offset_(r.offset_)
    {}

    template <size_t... column_numbers>
        requires((column_numbers < sizeof...(table_column_numbers)) && ...)
    [[nodiscard]] constexpr row<
        table_type,
        std::index_sequence<table_column_number(column_numbers)...>>
    narrow() const;

    template <typename... column_types>
    [[nodiscard]] constexpr row<
        table_type,
        std::index_sequence<
            table_type::template column_type_number<column_types>...>>
    narrow() const
        requires(has_type<column_types> && ...);

    [[nodiscard]] table_type::row_id row_id() const;

    template <size_t column_number>
        requires(column_number < sizeof...(table_column_numbers))
    friend auto& get(row r)
    {
        constexpr auto table_column = table_column_number(column_number);
        return std::get<table_column>(r.table_->data_)[r.offset_];
    }

    template <typename column_type>
        requires(type_is_one_of<column_type,
                                typename table_type::template column_type<
                                    table_column_numbers>...>)
    friend auto& get(row r)
    {
        constexpr size_t table_column
            = table_type::template column_type_number<column_type>;
        auto& element = std::get<table_column>(r.table_->data_)[r.offset_];
        return std::forward_like<table_type&>(element);
    }

    template <typename... column_types>
    bool operator==(const std::tuple<column_types...>& rh) const;

    template <typename T2>
        requires(std::is_same_v<const T2&, const table_type&>)
    bool
    operator==(const row<T2, std::index_sequence<table_column_numbers...>>& rh)
        const noexcept;

private:
    row(table_type* table, size_t offset);

    table_type* table_ = nullptr;
    size_t offset_ = 0;
};

template <typename>
constexpr bool is_row_v = false;

template <typename Table, size_t... column_numbers>
constexpr bool is_row_v<row<Table, std::index_sequence<column_numbers...>>>
    = true;

template <typename R>
concept row_type = is_row_v<R>;

template <typename R>
concept row_range = is_row_v<decltype(*std::declval<R&>().begin())>;

template <nothrow_movable... column_types>
class table {
    template <typename, typename>
    friend class row;

    using column_numbers = std::index_sequence_for<column_types...>;

public:
    template <size_t column_number>
    using column_type
        = std::tuple_element_t<column_number, std::tuple<column_types...>>;
    template <typename column_type>
    static constexpr size_t column_type_number
        = columnist::type_index<column_type, column_types...>;

    using row = columnist::row<table, column_numbers>;
    using const_row = columnist::row<const table, column_numbers>;

    struct row_id;
    template <typename>
    class iterator_t;

    using iterator = iterator_t<table>;
    using const_iterator = iterator_t<const table>;

    struct sentinel {};

    [[nodiscard]] size_t size() const { return rindex_.size(); }

    [[nodiscard]] bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> row_id;

    void erase(row_id);

    void erase(const_iterator i);

    [[nodiscard]] bool has_row_id(row_id k) const;

    [[nodiscard]] row operator[](row_id k);

    [[nodiscard]] const_row operator[](row_id k) const;

    [[nodiscard]] iterator begin();
    [[nodiscard]] const_iterator begin() const;
    [[nodiscard]] const_iterator cbegin() const;
    [[nodiscard]] sentinel end() const;
    [[nodiscard]] sentinel cend() const;

    void reserve(size_t size);
    [[nodiscard]] size_t capacity() const;

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

template <nothrow_movable... column_types>
template <typename T>
class table<column_types...>::iterator_t {
    friend class table<column_types...>;

public:
    using value_type = columnist::row<T, typename T::column_numbers>;
    using difference_type = ptrdiff_t;

    iterator_t() = default;

    operator iterator_t<const T>() const noexcept;

    template <typename TT>
        requires(std::is_same_v<const TT, T>)
    iterator_t& operator=(const iterator_t<TT>& tt) noexcept;

    template <typename TT>
        requires(std::is_same_v<const TT, const T>)
    bool operator==(const iterator_t<TT>& rh) const noexcept;

    bool operator==(sentinel) const noexcept;

    template <typename self>
        requires(!std::is_const_v<self>)
    self& operator++(this self&) noexcept;

    iterator_t operator++(int) noexcept;

    [[nodiscard]] value_type operator*() const noexcept;

private:
    iterator_t(T* table, size_t offset);

    T* table_ = nullptr;
    size_t offset_ = 0;
};

template <nothrow_movable... column_types>
struct table<column_types...>::row_id {
    row_id next_generation() const noexcept;

    explicit constexpr row_id(uint32_t i, uint8_t g = 0) noexcept;

    bool operator==(const row_id&) const = default;

    uint32_t offset: 24;
    uint8_t generation;
};

template <typename table_type, size_t... table_column_numbers>
constexpr auto
row<table_type,
    std::index_sequence<table_column_numbers...>>::table_column_number(size_t i)
    -> size_t
{
    constexpr std::array columns{ table_column_numbers... };
    return columns[i];
}

template <typename table_type, size_t... table_column_numbers>
template <size_t... column_numbers>
    requires((column_numbers < sizeof...(table_column_numbers)) && ...)
constexpr auto
row<table_type, std::index_sequence<table_column_numbers...>>::narrow() const
    -> row<table_type,
           std::index_sequence<table_column_number(column_numbers)...>>
{
    return { table_, offset_ };
}

template <typename table_type, size_t... table_column_numbers>
template <typename... column_types>
[[nodiscard]] constexpr auto
row<table_type, std::index_sequence<table_column_numbers...>>::narrow() const
    -> row<table_type,
           std::index_sequence<
               table_type::template column_type_number<column_types>...>>
    requires(has_type<column_types> && ...)

{
    return { table_, offset_ };
}

template <typename table_type, size_t... table_column_numbers>
auto row<table_type, std::index_sequence<table_column_numbers...>>::row_id()
    const -> table_type::row_id
{
    return table_->rindex_[offset_];
}

template <typename table_type, size_t... table_column_numbers>
template <typename... column_types>
auto row<table_type, std::index_sequence<table_column_numbers...>>::operator==(
    const std::tuple<column_types...>& rh) const -> bool
{
    auto elementwise_equality = [this](const auto&... vs) {
        return ((std::get<table_column_numbers>(table_->data_)[offset_] == vs)
                && ...);
    };
    return std::apply(elementwise_equality, rh);
}

template <typename table_type, size_t... table_column_numbers>
template <typename T2>
    requires(std::is_same_v<const T2&, const table_type&>)
auto row<table_type, std::index_sequence<table_column_numbers...>>::operator==(
    const row<T2, std::index_sequence<table_column_numbers...>>& rh)
    const noexcept -> bool
{
    return table_ == rh.table_ && offset_ == rh.offset_;
}

template <typename table_type, size_t... table_column_numbers>
row<table_type, std::index_sequence<table_column_numbers...>>::row(
    table_type* table, size_t offset)
: table_(table), offset_(offset)
{}

template <nothrow_movable... column_types>
template <typename... Us>
auto table<column_types...>::insert(Us&&... us) -> row_id
{
    auto data_idx = static_cast<uint32_t>(rindex_.size());
    auto push = [&]<size_t... Is, typename T>(std::index_sequence<Is...>,
                                              T&& t) {
        if constexpr ((std::is_nothrow_constructible_v<column_types, Us>
                       && ...)) {
            ((void)std::get<Is>(data_).emplace_back(
                 std::get<Is>(std::forward<T>(t))),
             ...);
        } else {
            size_t column = 0;
            try {
                (((void)std::get<Is>(data_).emplace_back(
                      std::get<Is>(std::forward<T>(t))),
                  column = Is + 1),
                 ...);

            } catch (...) {
                ((Is < column ? std::get<Is>(data_).pop_back() : throw), ...);
            }
        }
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

template <nothrow_movable... column_types>
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

template <nothrow_movable... column_types>
void table<column_types...>::erase(const_iterator i)
{
    assert(i.table_ == this);
    erase(rindex_[i.offset_]);
}

template <nothrow_movable... column_types>
bool table<column_types...>::has_row_id(row_id k) const
{
    if (k.offset >= index_.size()) { return false; }
    auto data_offset = index_[k.offset].offset;
    if (data_offset >= rindex_.size()) { return false; }
    return rindex_[data_offset] == k;
}

template <nothrow_movable... column_types>
auto table<column_types...>::operator[](row_id k) -> row
{
    assert(has_row_id(k));
    return { this, index_[k.offset].offset };
}

template <nothrow_movable... column_types>
auto table<column_types...>::operator[](row_id k) const -> const_row
{
    assert(has_row_id(k));
    return { this, index_[k.offset].offset };
}

template <nothrow_movable... column_types>
auto table<column_types...>::begin() -> iterator
{
    return { this, 0 };
}

template <nothrow_movable... column_types>
auto table<column_types...>::begin() const -> const_iterator
{
    return { this, 0 };
}

template <nothrow_movable... column_types>
auto table<column_types...>::cbegin() const -> const_iterator
{
    return { this, 0 };
}

template <nothrow_movable... column_types>
auto table<column_types...>::end() const -> sentinel
{
    return {};
}

template <nothrow_movable... column_types>
auto table<column_types...>::cend() const -> sentinel
{
    return {};
}

template <nothrow_movable... column_types>
void table<column_types...>::reserve(size_t size)
{
    std::invoke(
        [&]<size_t... Is>(std::index_sequence<Is...>) {
            (std::get<Is>(data_).reserve(size), ...);
            index_.reserve(size);
        },
        column_numbers{});
}

template <nothrow_movable... column_types>
auto table<column_types...>::capacity() const -> size_t
{
    return index_.capacity();
}

template <nothrow_movable... column_types>
template <typename T>
table<column_types...>::iterator_t<T>::operator iterator_t<const T>()
    const noexcept
{
    return { table_, offset_ };
}

template <nothrow_movable... column_types>
template <typename T>
template <typename TT>
    requires(std::is_same_v<const TT, T>)
auto table<column_types...>::iterator_t<T>::operator=(
    const iterator_t<TT>& tt) noexcept -> iterator_t&
{
    table_ = tt.table_;
    offset_ = tt.offset_;
}

template <nothrow_movable... column_types>
template <typename T>
template <typename TT>
    requires(std::is_same_v<const TT, const T>)
auto table<column_types...>::iterator_t<T>::operator==(
    const iterator_t<TT>& rh) const noexcept -> bool
{
    return table_ == rh.table_ && offset_ == rh.offset_;
}

template <nothrow_movable... column_types>
template <typename T>
auto table<column_types...>::iterator_t<T>::operator==(sentinel) const noexcept
    -> bool
{
    return offset_ == table_->size();
}

template <nothrow_movable... column_types>
template <typename T>
template <typename self>
    requires(!std::is_const_v<self>)
auto table<column_types...>::iterator_t<T>::operator++(this self& s) noexcept
    -> self&
{
    ++s.offset_;
    return s;
}

template <nothrow_movable... column_types>
template <typename T>
auto table<column_types...>::iterator_t<T>::operator++(int) noexcept
    -> iterator_t
{
    auto copy = *this;
    ++*this;
    return copy;
}

template <nothrow_movable... column_types>
template <typename T>
auto table<column_types...>::iterator_t<T>::operator*() const noexcept
    -> value_type
{
    return value_type(table_, offset_);
}

template <nothrow_movable... column_types>
template <typename T>
table<column_types...>::iterator_t<T>::iterator_t(T* table, size_t offset)
: table_(table), offset_(offset)
{}

template <nothrow_movable... column_types>
auto table<column_types...>::row_id::next_generation() const noexcept -> row_id
{
    auto copy = *this;
    ++copy.generation;
    return copy;
}

template <nothrow_movable... column_types>
constexpr table<column_types...>::row_id::row_id(uint32_t i, uint8_t g) noexcept
: offset(i & ((1U << 24) - 1)), generation(g)
{}

template <size_t... selected_columns, typename R>
    requires row_type<std::remove_cvref_t<R>>
[[nodiscard]] constexpr auto select(R&& r)
{
    return r.template narrow<selected_columns...>();
}

template <typename... selected_types, typename R>
    requires row_type<std::remove_cvref_t<R>>
[[nodiscard]] constexpr auto select(R&& r)
{
    return r.template narrow<selected_types...>();
}

template <size_t... selected_column_numbers, typename function>
    requires(not is_row_v<function> && not row_range<function>)
[[nodiscard]] constexpr auto select(function f)
{
    return [f = std::move(f)]<typename self>(
               this self&&, row_type auto r) -> decltype(auto) {
        return std::forward_like<self>(f)(
            select<selected_column_numbers...>(r));
    };
}

template <typename... selected_types, typename function>
    requires(not is_row_v<function> && not row_range<function>)
[[nodiscard]] constexpr auto select(function f)
{
    return [f = std::move(f)]<typename self>(
               this self&&, row_type auto r) -> decltype(auto) {
        return std::forward_like<self>(f)(select<selected_types...>(r));
    };
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

        using difference_type = ptrdiff_t;
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

template <size_t... column_numbers>
struct [[nodiscard]] range_selector_maker {
    template <row_range_with_column_numbers<column_numbers...> R>
    friend range_index_selector<R, column_numbers...>
    operator|(R& r, range_selector_maker)
    {
        return { r };
    }
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

        using difference_type = ptrdiff_t;
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

template <typename... column_types>
struct [[nodiscard]] range_type_selector_maker {
    template <row_range_with_column_types<column_types...> R>
    friend range_type_selector<R, column_types...>
    operator|(R& r, range_type_selector_maker)
    {
        return { r };
    }
};

template <typename... column_types>
range_type_selector_maker<column_types...> select()
{
    return {};
}

} // namespace columnist

template <std::size_t I, typename Table, size_t... column_numbers>
struct std::tuple_element<
    I,
    columnist::row<Table, std::index_sequence<column_numbers...>>> {
    using type = columnist::forwarded_like_t<
        Table&,
        columnist::nth_type_t<
            I,
            typename Table::template column_type<column_numbers>...>&>;
};

template <typename Table, size_t... column_numbers>
struct std::tuple_size<
    columnist::row<Table, std::index_sequence<column_numbers...>>>
: std::integral_constant<size_t, sizeof...(column_numbers)> {};

#endif // COLUMNIST_TABLE_HPP
