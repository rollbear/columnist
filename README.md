![columnist-logo](columnist-logo.png)

[![CI Build Status](https://github.com/rollbear/columnist/actions/workflows/build.yml/badge.svg)](https://github.com/rollbear/columnist/actions/workflows/build.yml)
[![codecov](https://codecov.io/gh/rollbear/columnist/graph/badge.svg?token=GVJMZUOC5G)](https://codecov.io/gh/rollbear/columnist)

<a href="https://www.buymeacoffee.com/bjornfahller"> <img src="https://cdn.buymeacoffee.com/buttons/v2/default-orange.png" height="50" width="210" alt="Buy me a coffee"/></a>



**columnist** is an experimental C++23 library for
[ECS](https://en.wikipedia.org/wiki/Entity_component_system)
(Entity Component System), a data structure based on "struct
of vectors" for good locality of reference. Each "vector" is
column. Elements are kept packed towards the beginning of each
column, but can be referenced using stable row_id type. The
elements at the same row_id for each column is a row.

Status: **Highly Experimental**

## Examples

### General usage: [![Static Badge](https://img.shields.io/badge/compiler%20explorer%20-%20?logo=Compiler%20Explorer&logoColor=%23000000)](https://godbolt.org/z/1TbfTssf1)

```C++
#include <columnist/table.hpp>
#include <columnist/functional.hpp>

#include <ranges>
#include <algorithm>
#include <cassert>
#include <print>

int main() {
    // Create a table with 3 column_types
    columnist::table<int, float, std::string> values;

    // insertion returns row IDs, which can be used for direct access and removal
    auto pi_id = values.insert(1, 3.1416, "pi");
    auto e_id = values.insert(2, 2.7813, "e");

    // select a subset of column_types by type or by index
    auto pi_num = columnist::select<float>(values[pi_id]); // float 3.1416
    auto e_str = columnist::select<2>(values[e_id]);       // std::string("e")

    // a table, and a column selection of a table, integrates well with ranges
    std::ranges::for_each(values | columnist::select<std::string,float>(),
                          columnist::apply(
                              [](auto name, auto num){
                                  std::println("{}=\t{}", name, num);
                              })
                          );
    // prints
    // pi=    3.1416
    // e=     2.7813

    constexpr auto less_than = [](auto rh) {
        return [rh](auto lh) { return lh < rh;};
    };

    erase_if(values,
             columnist::select<0>(columnist::apply(less_than(2))));

    // values now only holds e

    values.erase(e_id); // values is now empty

    assert(values.empty());
}
```

### Code generation with columninist and [strong_type](https://github.com/rollbear/strong_type) [![Static Badge](https://img.shields.io/badge/compiler%20explorer%20-%20?logo=Compiler%20Explorer&logoColor=%23000000)](https://godbolt.org/z/rbsY8sjW5)

```C++
#include <strong_type/type.hpp>
#include <strong_type/affine_point.hpp>
#include <strong_type/ordered.hpp>

#include <columnist/functional.hpp>
#include <columnist/table.hpp>

#include <ranges>
#include <algorithm>
#include <chrono>

template <typename tag>
using acceleration = strong::type<float, tag>;
template <typename tag>
using velocity = strong::type<float, tag, strong::difference>;
template <typename tag, typename ... mods>
using distance = strong::type<float, tag, strong::difference, mods...>;
template <typename tag, typename delta, typename ... mods>
using pos = strong::type<float, tag, strong::affine_point<delta>, mods...>;

template <typename tag>
inline auto operator*(acceleration<tag> a, std::chrono::seconds t)
{
    return velocity<tag>{value_of(a)*t.count()};
}

template <typename tag>
inline auto operator*(std::chrono::seconds t, acceleration<tag> a) { return a * t; }

template <typename tag>
inline auto operator*(velocity<tag> v, std::chrono::seconds t)
{
    return distance<tag>{value_of(v) * t.count()};
}

template <typename tag>
inline auto operator*(std::chrono::seconds t, velocity<tag> v) { return v * t; }


using acceleration_y = acceleration<struct ytag>;
using velocity_x = velocity<struct xtag>;
using velocity_y = velocity<struct ytag>;
using delta_x = distance<struct xtag>;
using delta_y = distance<struct ytag>;
using pos_x = pos<struct xtag, delta_x, strong::ordered>;
using pos_y = pos<struct ytag, delta_y, strong::ordered>;
using delta_x = strong::type<float, struct xtag, strong::difference>;


using objects = columnist::table<pos_x, pos_y, velocity_x, velocity_y>;


static inline auto less_equal = [](auto x) { return [x](auto y) { return y <= x;}; };

template <typename ... column_types>
inline auto abs(strong::type<column_types...> v) { value_of(v) = abs(value_of(v)); return v; }

void remove_stopped_objetcts(objects& objs)
{
    constexpr auto stopped = [](auto dx, auto dy){ return abs(dx) < delta_x(0.01) && abs(dy) < delta_y(0.01);};
    erase_if(objs, columnist::select<delta_x, delta_y>(columnist::apply(stopped)));
}


void bounce_on_floor(objects& objs)
{
    constexpr pos_y floor{0.0};
    auto objs_on_floor = objs
                       | std::ranges::views::filter(columnist::select<pos_y>(columnist::apply(less_equal(floor))));
    for (auto [dy] : objs_on_floor | columnist::select<delta_y>()) {
        dy *= -0.95;
    }
}


void update_pos(objects& objs, acceleration_y a, std::chrono::seconds t)
{
    std::ranges::for_each(objs | columnist::select<pos_y, velocity_y>(),
                          columnist::apply([a,t](auto& y, auto& vy) { vy+= a*t; y+= vy*t;}));
    std::ranges::for_each(objs | columnist::select<pos_x, velocity_x>(),
                          columnist::apply([t](auto& x, auto vx){x+= vx*t;}));
}

```

# Documentation

## `<columnist/table.hpp>`

### Overview

```C++
namespace columnist {
template <typename ... column_types>
class table {
public:
    struct row_id {
        uint32_t index:24;
        uint8_t generation;
    };
    using row = ...
    using const_row = ...
    class iterator;
    class const_iterator;
    class sentinel;
    
    size_t size() const;
    bool empty() const;
    template <typename ... Us>
        requires(std::is_constructible_v<column_types, Us> && ...)
    row_id insert(Us&& ... us);
    
    void erase(row_id);
    void erase(const_iterator);
    
    bool has_row_id(row_id) const;
    
    row operator[](row_id);
    const_row operator[](row_id) const;
    
    iterator begin();
    const_iterator begin() const;
    const_iterator cbegin() const;
    sentinel end() const;
    sentinel cend() const;
    
    template <typename Predicate>
    friend size_t erase_if(table&, Predicate);
};

template <typename Table, std::index_sequence<selected_column_numbers...>>
class row {
public:
    row();
    template <size_t ... PIs>
    explicit row(const row<Table, std::index_sequence<PIs...>>&) noexcept;
    
    Table::row_id row_id() const;
    
    template <size_t I>
    friend decltype(auto) get(row r);
    template <typename T>
    friend decltype(auto) get(row r);
    
    template <typename ... column_types>
    bool operator==(const tuple<column_types...>&) const;
    template <typename Table2>
        requires(is_same_v<const Table, const Table2>)
    bool operator==(const row<Table2, std::index_sequence<selected_column_numbers...>>& rh) const noexcept;
};
```

### Concepts

#### `row_range`

A range type whose iterators return a table `row` type.

### `row_range_with_column_numbers<selected_column_numbers...>`

A `row_range` for which `select<selected_column_numbers...>` will work

### `row_range_with_column_types<column_types...>`

A `row_range` for which `select<column_types...> will work`

### Free functions

#### `template <size_t ... selected_column_numbers> constexpr auto select<selected_column_numbers...>(row r)`

Returns a row with the `selected_column_numbers...` elements of `r`. Note that each value `selected_column_numbers` refers
to the number of column_types referred to by `r`, not the column_types of the owning
table, therefore `select()` can only be used to narrow a row to a subset of the
elements referred to by `r`.

#### `template <typename ... column_types> constexpr auto select<column_types...>(row r)`

Returns a row with the `column_types...` types from `r`. Note that each type `column_types` refers
to the types referred to by `r`, not the types of the owning table, therefore
`select()` can only be used to narrow a row to a subset of the elements referred
to by `r`.

#### `template <size_t ... selected_column_numbers, typename F> constexpr auto select<selected_column_numbers...>(F f)`

Returns a callable that takes a `row` `r`, and calls `f(get<selected_column_numbers>(r)...)`

The function `f` must be callable with a `row` with `selected_column_numbers` column_numbers.

#### `template <typename ... column_types, typename F> constexpr auto select<column_types...>(F f)`

Returns a callable that takes a `row` r, and calls `f(std::get<column_types>(r)...)`

The function `f` must be callable with a `row` with `column_types` members.

#### `template <size_t ... selected_column_numbers> select<selected_column_numbers...>(row_range& r)`

Returns a range spanning the same elements as `r`, but with a
subselection of column_types from the column_numbers `selected_column_numbers`.

#### `template <size_t ... selected_column_numbers> operator|(row_range& r, select<selected_column_numbers...>())`

Returns a range spanning the same elements as `r`, but with a
subselection of column_types from the column_numbers `selected_column_numbers`.

#### `template <typename ... column_types> select<column_types...>(row_range& r)`

Returns a range spanning the same elements as `r`, but with a
subselection of column_types from the types `column_types`.

#### `template <typename ... column_types> operator|(row_range r, select<column_types...>())`

Returns a range spanning the same elements as `r`, but with a
subselection of column_types from the types `column_types`.

## `<columnist/functional.hpp`

### Overview

#### `inline constexpr apply = []<typename F>(F&&f)`

Higher order function generalizing `std::apply()`.

If `f` is a function accepting `column_types...` as arguments, then `apply(f)` is
callable with a type `T` for which `get<selected_column_numbers>()` returns a type matching `column_types` for
all column_numbers. In particular, it is callable with `columnist::row<>`,
`std::tuple<column_types...>` or something that inherits from `std::tuple<column_types...>`.

