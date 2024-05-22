#ifndef STORE_STORE_HPP
#define STORE_STORE_HPP

#include <cassert>
#include <cstdint>
#include <functional>
#include <tuple>
#include <vector>

template <typename... Ts>
class store {
public:
    struct key {
        key next_generation() const
        {
            auto copy = *this;
            ++copy.generation;
            return copy;
        }

        constexpr key(uint32_t i, uint8_t g = 0)
        : index(i & ((1U << 24) - 1)), generation(g)
        {}

        bool operator==(const key&) const = default;
        uint32_t index: 24;
        uint8_t generation;
    };
    struct iterator;
    using value_type = std::tuple<const key&, Ts&...>;

    size_t size() const { return rindex_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Us>
    auto insert(Us&&... us) -> iterator
        requires(std::is_constructible_v<Ts, Us> && ...);

    void erase(key);
    void erase(iterator i);

    bool has_key(key k) const;

    std::tuple<Ts&...> operator[](key k) { return lookup(*this, k); }

    const std::tuple<Ts&...> operator[](key k) const
    {
        return lookup(*this, k);
    }

    iterator begin();
    iterator end();

private:
    template <typename S>
    static auto lookup(S&& s, key k)
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

    std::tuple<std::vector<Ts>...> data_;
    std::vector<key> rindex_;
    std::vector<key> index_;
    key first_free_ = { 0, 0 };
};

template <typename... Ts>
class store<Ts...>::iterator {
    friend class store<Ts...>;

public:
    using iterator_category = std::forward_iterator_tag;
    bool operator==(const iterator&) const = default;

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

    store<Ts...>::value_type operator*() const
    {
        auto get = [&]<size_t... Is>(std::index_sequence<Is...>) {
            return std::forward_as_tuple(s->rindex_[idx],
                                         std::get<Is>(s->data_)[idx]...);
        };
        return get(std::make_index_sequence<sizeof...(Ts)>{});
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
auto store<Ts...>::begin() -> iterator
{
    return { this, 0 };
}

template <typename... Ts>
auto store<Ts...>::end() -> iterator
{
    return { this, rindex_.size() };
}

template <typename... Ts>
template <typename... Us>
auto store<Ts...>::insert(Us&&... us) -> iterator
    requires(std::is_constructible_v<Ts, Us> && ...)
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
        first_free_ = key(static_cast<uint32_t>(index_.size()));
    } else {
        auto index_pos = first_free_;
        first_free_ = index_[first_free_.index];
        index_[index_pos.index] = { data_idx, index_pos.generation };
        rindex_.push_back(index_pos);
    }
    return iterator{ this, data_idx };
}

template <typename... Ts>
void store<Ts...>::erase(key k)
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
void store<Ts...>::erase(iterator i)
{
    assert(i.s == this);
    erase(rindex_[i.idx]);
}

template <typename... Ts>
bool store<Ts...>::has_key(key k) const
{
    if (k.index >= index_.size()) { return false; }
    auto data_idx = index_[k.index].index;
    if (data_idx >= rindex_.size()) { return false; }
    return rindex_[data_idx] == k;
}
#endif // STORE_STORE_HPP
