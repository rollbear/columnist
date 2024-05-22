#ifndef STORE_STORE_HPP
#define STORE_STORE_HPP

#include <cassert>
#include <cstdint>
#include <functional>
#include <tuple>
#include <vector>

namespace table {

namespace column {
template <size_t N>
struct name {
    consteval name(const char* p)
    {
        std::copy_n(p, N, data_);
        data_[N] = '\0';
    }

    template <size_t M>
    consteval bool operator==(name<M> rh) const
    {
        return std::equal(data_, data_ + N, rh.data_, rh.data_ + M);
    }

    char data_[N + 1];
};

template <size_t N>
name(const char (&)[N]) -> name<N - 1>;

struct index {
    consteval index(size_t n)
    : value(n)
    {}

    size_t value;
};

template <typename T>
struct id {
    consteval id(size_t n)
        requires(std::is_same_v<T, index>)
    : value(n)
    {}

    template <size_t N>
    consteval id(const char (&p)[N])
        requires(std::is_same_v<T, name<N - 1>>)
    : value(p)
    {}

    T value;
};

id(size_t) -> id<index>;
template <size_t N>
id(const char (&)[N]) -> id<name<N - 1>>;

template <name n, typename T>
struct named_type {};
} // namespace column

namespace internal {

template <typename T>
struct type_of {
    using type = T;
};

template <column::name n, typename T>
struct type_of<column::named_type<n, T>> {
    using type = T;
};

template <typename T>
using type_of_t = typename type_of<T>::type;

static_assert(std::is_same_v<int, type_of_t<int>>);
static_assert(std::is_same_v<int, type_of_t<column::named_type<"foo", int>>>);

template <column::name n, typename T>
struct has_name : std::false_type {};

template <column::name n, typename T>
inline constexpr bool has_name_v = has_name<n, T>::value;

template <column::name desired, column::name actual, typename T>
struct has_name<desired, column::named_type<actual, T>>
: std::bool_constant<desired == actual> {};

static_assert(!has_name_v<"foo", bool>);
static_assert(!has_name_v<"foo", column::named_type<"barf", bool>>);
static_assert(has_name_v<"foo", column::named_type<"foo", bool>>);

template <typename T, typename, typename... Ts>
static constexpr size_t type_index = 1 + type_index<T, Ts...>;

template <typename T, typename... Ts>
static constexpr size_t type_index<T, T, Ts...> = 0;

template <auto, typename, typename... Ts>
struct column_index;

template <column::name n, typename T, typename... Ts>
struct column_index<n, T, Ts...> {
    static constexpr size_t value = 1 + column_index<n, Ts...>::value;
};

template <column::name desired, typename T, typename... Ts>
struct column_index<desired, column::named_type<desired, T>, Ts...> {
    static constexpr size_t value = 0;
};

template <column::index i, typename T, typename... Ts>
struct column_index<i, T, Ts...> {
    static constexpr size_t value = i.value;
};

template <column::id n, typename... Ts>
inline constexpr size_t column_index_v = column_index<n.value, Ts...>::value;

static_assert(column_index_v<"foo",
                             int,
                             column::named_type<"bor", void>,
                             column::named_type<"foo", int>,
                             char>
              == 2);
static_assert(column_index_v<3,
                             int,
                             column::named_type<"bor", void>,
                             column::named_type<"foo", int>,
                             char>
              == 3);

} // namespace internal

template <typename... Ts>
class store {
public:
    struct handle {
        handle next_generation() const
        {
            auto copy = *this;
            ++copy.generation;
            return copy;
        }

        constexpr handle(uint32_t i, uint8_t g = 0)
        : index(i & ((1U << 24) - 1)), generation(g)
        {}

        bool operator==(const handle&) const = default;
        uint32_t index: 24;
        uint8_t generation;
    };
    template <size_t... Is>
    struct iterator;

    struct sentinel {
        size_t size;
    };

    using value_type = std::tuple<const handle&, Ts&...>;

    size_t size() const { return rindex_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> handle
        requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...);

    void erase(handle);
    template <size_t... Is>
    void erase(iterator<Is...> i);

    bool has_key(handle k) const;

    std::tuple<Ts&...> operator[](handle k) { return lookup(*this, k); }

    const std::tuple<Ts&...> operator[](handle k) const
    {
        return lookup(*this, k);
    }

    template <column::id... ids>
    iterator<internal::column_index_v<ids, Ts...>...> begin();
    iterator<> begin();
    sentinel end();

    template <column::id... ids>
    friend auto select(store& s)
    {
        return selector<internal::column_index_v<ids, Ts...>...>{ &s };
    }

    template <typename... Us>
    friend auto select(store& s)
    {
        return selector<internal::type_index<Us, Ts...>...>{ &s };
    }

private:
    template <size_t... Is>
    struct selector {
        auto begin() { return s->begin<Is...>(); }

        auto end() { return s->end(); }

        store<Ts...>* s;
    };

    template <typename S>
    static auto lookup(S&& s, handle k)
    {
        assert(k.index < s.index_.size());
        auto data_idx = s.index_[k.index].index;
        auto get = [&]<size_t... Is>(std::index_sequence<Is...>,
                                     uint32_t data_idx) {
            return std::forward_as_tuple(std::get<Is>(s.data_)[data_idx]...);
        };
        return std::invoke(
            get, std::make_index_sequence<sizeof...(Ts)>{}, data_idx);
    }

    std::tuple<std::vector<internal::type_of_t<Ts>>...> data_;
    std::vector<handle> rindex_;
    std::vector<handle> index_;
    handle first_free_ = { 0, 0 };
};

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
        auto get = [&]<size_t... Is>(std::index_sequence<Is...>) {
            return std::forward_as_tuple(s->rindex_[idx],
                                         std::get<Is>(s->data_)[idx]...);
        };
        if constexpr (sizeof...(Idxs) == 0) {
            return get(std::make_index_sequence<sizeof...(Ts)>{});
        } else {
            return get(std::index_sequence<Idxs...>{});
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
template <column::id... ids>
auto store<Ts...>::begin() -> iterator<internal::column_index_v<ids, Ts...>...>
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
auto store<Ts...>::insert(Us&&... us) -> handle
    requires(std::is_constructible_v<internal::type_of_t<Ts>, Us> && ...)
{
    auto data_idx = static_cast<uint32_t>(rindex_.size());
    auto push = [&]<size_t... Is>(std::index_sequence<Is...>, auto&& t) {
        (std::get<Is>(data_).push_back(std::get<Is>(std::move(t))), ...);
    };
    std::invoke(push,
                std::make_index_sequence<sizeof...(Ts)>{},
                std::forward_as_tuple(std::forward<Us>(us)...));
    if (first_free_.index == index_.size()) {
        rindex_.push_back(first_free_);
        index_.push_back({ data_idx, 0 });
        first_free_ = handle(static_cast<uint32_t>(index_.size()));
        return index_.back();
    } else {
        auto index_pos = std::exchange(first_free_, index_[first_free_.index]);
        index_[index_pos.index] = { data_idx, index_pos.generation };
        rindex_.push_back(index_pos);
        return index_pos;
    }
}

template <typename... Ts>
void store<Ts...>::erase(handle k)
{
    assert(k.index < index_.size());
    auto data_idx = index_[k.index].index;
    assert(data_idx < rindex_.size());
    assert(rindex_[data_idx].index == k.index);
    auto move_last = [&]<size_t... Is>(std::index_sequence<Is...>) {
        ((std::get<Is>(data_)[data_idx] = std::move(std::get<Is>(data_).back()),
          std::get<Is>(data_).pop_back()),
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
bool store<Ts...>::has_key(handle k) const
{
    if (k.index >= index_.size()) { return false; }
    auto data_idx = index_[k.index].index;
    if (data_idx >= rindex_.size()) { return false; }
    return rindex_[data_idx] == k;
}
} // namespace table
#endif // STORE_STORE_HPP
