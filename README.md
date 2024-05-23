[![CI Build Status](https://github.com/rollbear/columnist/actions/workflows/build.yml/badge.svg)](https://github.com/rollbear/columnist/actions/workflows/build.yml)
[![codecov](https://codecov.io/gh/rollbear/columnist/graph/badge.svg?token=GVJMZUOC5G)](https://codecov.io/gh/rollbear/columnist)

**columnist** is an experimental C++23 library for
[ECS](https://en.wikipedia.org/wiki/Entity_component_system)
(Entity Component System), a data structure based on "struct
of vectors" for good locality of reference. Each "vector" is
column. Elements are kept packed towards the beginning of each
column, but can be referenced using stable row_id type. The
elements at the same row_id for each column is a row.

Status: **Highly Experimental**

### Examples

#### General usage: [Compiler Explorer](https://godbolt.org/z/1TbfTssf1)
```C++
#include <columnist/table.hpp>
#include <columnist/functional.hpp>

#include <ranges>
#include <algorithm>
#include <cassert>
#include <print>

int main() {
    // Create a table with 3 columns
    columnist::table<int, float, std::string> values;

    // insertion returns row IDs, which can be used for direct access and removal
    auto pi_id = values.insert(1, 3.1416, "pi");
    auto e_id = values.insert(2, 2.7813, "e");

    // select a subset of columns by type or by index
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

#### Code generation with columninist and [strong_type](https://github.com/rollbear/strong_type) [Compiler Explorer](https://godbolt.org/z/bMKPxGTW7)

```C++
#include <columnist/table.hpp>
#include <columnist/functional.hpp>

#include <strong_type/type.hpp>
#include <strong_type/affine_point.hpp>
#include <strong_type/ordered.hpp>
#include <strong_type/scalable_with.hpp>


#include <ranges>
#include <algorithm>

using acceleration = strong::type<float, struct atag>;
using delta_x = strong::type<float, struct xtag, strong::difference>;
using delta_y = strong::type<float, struct ytag, strong::difference, strong::scalable_with<acceleration>>;
using pos_x = strong::type<float, struct xtag, strong::affine_point<delta_x>, strong::ordered>;
using pos_y = strong::type<float, struct ytag, strong::affine_point<delta_y>, strong::ordered>;

using objects = columnist::table<pos_x, pos_y, delta_x, delta_y>;

static inline auto less_equal = [](auto x) {
    return [x](auto y) { return y <= x;};
};

void update_pos(objects& objs, acceleration a)
{
    std::ranges::for_each(objs | columnist::select<pos_y, delta_y>(),
                          columnist::apply([a](auto& y, auto& dy) { dy*= a; y+= dy;}));
    std::ranges::for_each(objs | columnist::select<pos_x, delta_x>(),
                          columnist::apply([](auto& x, auto dx){x+= dx;}));
}

void bounce_on_floor(objects& objs)
{
    constexpr pos_y floor{0.0};
    auto objs_on_floor = objs
                       | std::ranges::views::filter(columnist::select<pos_y>(columnist::apply(less_equal(floor))));
    for (auto [dy] : objs_on_floor | columnist::select<delta_y>()) {
        value_of(dy) = -0.95*value_of(dy);
    }
}

void remove_stopped_objetcts(objects& objs)
{
    constexpr auto stopped = [](auto dx, auto dy){ return abs(value_of(dx)) < 0.01 && abs(value_of(dy)) < 0.01;};
    erase_if(objs, columnist::select<delta_x, delta_y>(columnist::apply(stopped)));
}
```
