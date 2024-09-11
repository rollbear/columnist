#ifndef COLUMNIST_MULTIVECTOR_HPP
#define COLUMNIST_MULTIVECTOR_HPP

#include "type_utils.hpp"

#include <algorithm>
#include <bit>
#include <cstring>
#include <functional>
#include <memory>
#include <type_traits>

namespace columnist {
template <nothrow_movable... column_types>
struct multivector {
public:
    multivector() = default;
    multivector(const multivector& v);

    multivector(multivector&& v) noexcept
    : memory_(std::move(v.memory_))
    , capacity_(std::exchange(v.capacity_, 0))
    , size_(std::exchange(v.size_, 0))
    {}

    ~multivector()
    {
        auto destroy_columns = [&]<size_t... Is>(std::index_sequence<Is...>) {
            auto destroy_column
                = [&]<size_t I>(std::integral_constant<size_t, I>) {
                      if constexpr (!std::is_trivially_destructible_v<
                                        column_type<I>>) {
                          for (size_t offset = 0; offset != size(); ++offset) {
                              std::destroy_at(&get<I>(offset));
                          }
                      }
                  };
            ((destroy_column(std::integral_constant<size_t, Is>{}), ...));
        };
        destroy_columns(column_indexes);
    }

    multivector& operator=(const multivector v)
    {
        multivector copy{ v };
        swap(*this, copy);
        return *this;
    }

    multivector& operator=(multivector&& v) noexcept
    {
        memory_ = std::move(v.memory_);
        capacity_ = std::exchange(v.capacity_, 0);
        size_ = std::exchange(v.size_, 0);
        return *this;
    }

    friend void swap(multivector& lh, multivector& rh) noexcept
    {
        std::swap(lh.memory_, rh.memory_);
        std::swap(lh.capacity_, rh.capacity_);
        std::swap(lh.size_, rh.size_);
    }

    [[nodiscard]] size_t size() const { return size_; }

    [[nodiscard]] bool empty() const { return size() == 0; }

    [[nodiscard]] size_t capacity() const { return capacity_; }

    size_t reserve(size_t min_capacity)
    {
        auto desired_capacity = std::bit_ceil(min_capacity);
        grow(desired_capacity);
        return capacity();
    }

    template <typename... types>
    std::tuple<column_types&...> push_back(types&&... ts)
        requires(std::is_constructible_v<column_types, types> && ...)
    {
        if (size() == capacity()) { reserve(capacity_ * 2); }
        size_t constructed_column = 0;
        auto construct = [&]<size_t I, typename from_type>(
                             from_type&& from,
                             std::integral_constant<size_t, I>) -> auto& {
            auto& dst = get<I>(size_);
            std::construct_at(&dst, std::forward<from_type>(from));
            constructed_column = I;
            return dst;
        };
        if constexpr ((std::is_nothrow_constructible_v<column_types, types>
                       && ...)) {
            auto ret = std::invoke(
                [&]<size_t... Is>(std::index_sequence<Is...>) {
                    return std::forward_as_tuple(
                        construct(std::forward<types>(ts),
                                  std::integral_constant<size_t, Is>{})...);
                },
                column_indexes);
            ++size_;
            return ret;
        } else {
            try {
                auto ret = std::invoke(
                    [&]<size_t... Is>(std::index_sequence<Is...>) {
                        (construct(std::forward<types>(ts),
                                   std::integral_constant<size_t, Is>{}),
                         ...);
                        return std::forward_as_tuple(get<Is>(size_)...);
                    },
                    column_indexes);
                ++size_;
                return ret;
            } catch (...) {
                auto undo_construction
                    = [&]<size_t... Is>(std::index_sequence<Is...>) {
                          (((Is <= constructed_column)
                            ? std::destroy_at(&get<Is>(size_)),
                            0
                            : 0),
                           ...);
                      };
                undo_construction(column_indexes);
                throw;
            }
        }
    }

    template <typename self>
    std::tuple<forwarded_like_t<self, column_types&>...>
    operator[](this self&& obj, size_t offset)
    {
        return std::invoke(
            [&]<size_t... Is>(std::index_sequence<Is...>) {
                return std::forward_as_tuple(obj.template get<Is>(offset)...);
            },
            column_indexes);
    }

private:
    template <size_t I>
    using column_type = nth_type_t<I, column_types...>;

    template <size_t I, typename self>
    forwarded_like_t<self, column_type<I>&> get(this self&& obj, size_t offset)
    {
        return *static_cast<column_type<I>*>(static_cast<void*>(
            &obj.memory_[obj.template offset_of<I>(offset, obj.capacity_)]));
    }

    void grow(size_t new_capacity)
    {
        constexpr auto last_column = num_columns - 1;
        auto allocation_size
            = offset_of<last_column>(new_capacity, new_capacity);
        std::unique_ptr<char[], deleter> new_memory{
            static_cast<char*>(operator new(allocation_size, largest_alignment))
        };
        auto move_data = [&]<size_t... Is>(std::index_sequence<Is...>) {
            auto move_column = [&]<size_t I>(
                                   std::integral_constant<size_t, I>) {
                using T = column_type<I>;
                if constexpr (std::is_trivial_v<T>) {
                    auto& src = get<I>(0);
                    auto& dst = *static_cast<T*>(static_cast<void*>(
                        &new_memory[offset_of<I>(0, new_capacity)]));
                    std::memcpy(&dst, &src, size_ * sizeof(T));
                } else {
                    for (size_t offset = 0; offset != size_; ++offset) {
                        auto& src = get<I>(offset);
                        auto& dst = *static_cast<T*>(static_cast<void*>(
                            &new_memory[offset_of<I>(offset, new_capacity)]));
                        std::construct_at<T>(&dst, std::move(src));
                        if constexpr (!std::is_trivially_destructible_v<T>) {
                            std::destroy_at(&src);
                        }
                    }
                }
            };
            (move_column(std::integral_constant<size_t, Is>{}), ...);
        };
        if (!empty()) { move_data(column_indexes); }
        memory_ = std::move(new_memory);
        capacity_ = new_capacity;
    }

    template <size_t column>
    static constexpr size_t offset_of(size_t offset, size_t capacity)
    {
        auto base = std::invoke(
            [=]<size_t... Is>(std::index_sequence<Is...>) {
                size_t table_start = 0;
                size_t prev_size = 0;
                auto calc = [&]<size_t I>(std::integral_constant<size_t, I>) {
                    using T = nth_type_t<I, column_types...>;
                    auto rv = (table_start + capacity * prev_size
                               + (alignof(T) - 1))
                            & ~(alignof(T) - 1);
                    prev_size = sizeof(T);
                    return rv;
                };
                ((table_start = calc(std::integral_constant<size_t, Is>{})),
                 ...);
                return table_start;
            },
            std::make_index_sequence<column + 1>{});
        return base + offset * sizeof(column_type<column>);
    }

    struct deleter {
        void operator()(char* addr) const
        {
            operator delete(addr, largest_alignment);
        }
    };

    std::unique_ptr<char[], deleter> memory_ = nullptr;
    size_t capacity_ = 0;
    size_t size_ = 0;
    static constexpr auto num_columns = sizeof...(column_types);
    static constexpr auto largest_alignment
        = std::align_val_t(std::max({ alignof(column_types)... }));
    static constexpr auto column_indexes
        = std::index_sequence_for<column_types...>{};
};
} // namespace columnist

#endif // COLUMNIST_MULTIVECTOR_HPP
