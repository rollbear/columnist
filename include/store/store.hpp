#ifndef STORE_STORE_HPP
#define STORE_STORE_HPP

#include <cassert>
#include <cstdint>
#include <tuple>
#include <vector>

template <typename T>
class store {
public:
    struct key {
        bool operator==(const key&) const = default;
        uint32_t index: 24;
        uint32_t generation: 8;
    };
    struct iterator;
    using value_type = std::tuple<const key&, T&>;

    size_t size() const { return data_.size(); }

    bool empty() const { return size() == 0; }

    template <typename... Ts>
        requires std::is_constructible_v<T, Ts...>
    value_type insert(Ts&&... ts);

    void erase(key);

    bool has_key(key k) const;

    T& operator[](key k) { return lookup(*this, k); }

    const T& operator[](key k) const { return lookup(*this, k); }

    iterator begin();
    iterator end();

private:
    template <typename S>
    static auto& lookup(S&& s, key k)
    {
        assert(k.index < s.index_.size());
        auto data_idx = s.index_[k.index].index;
        return s.data_[data_idx];
    }

    std::vector<T> data_;
    std::vector<key> rindex_;
    std::vector<key> index_;
    uint32_t first_free_ = 0;
};

template <typename T>
class store<T>::iterator {
    friend class store<T>;

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

    store<T>::value_type operator*() const
    {
        return { s->rindex_[idx], s->data_[idx] };
    }

    iterator(){};

private:
    iterator(store<T>* s, size_t idx)
    : s(s), idx(idx)
    {}

    store<T>* s = nullptr;
    size_t idx = 0;
};

template <typename T>
auto store<T>::begin() -> iterator
{
    return { this, 0 };
}

template <typename T>
auto store<T>::end() -> iterator
{
    return { this, data_.size() };
}

template <typename T>
template <typename... Ts>
    requires std::is_constructible_v<T, Ts...>
auto store<T>::insert(Ts&&... ts) -> value_type
{
    auto data_idx = static_cast<uint32_t>(data_.size());
    auto& element = data_.emplace_back(std::forward<Ts>(ts)...);
    rindex_.push_back(key{ first_free_, 0 });
    if (first_free_ == index_.size()) {
        index_.push_back({ data_idx, 0 });
        first_free_ = index_.size();
    } else {
        auto index_pos = first_free_;
        first_free_ = index_[first_free_].index;
        index_[index_pos] = { data_idx, 0 };
    }
    return { rindex_.back(), element };
}

template <typename T>
void store<T>::erase(key k)
{
    assert(k.index < index_.size());
    auto data_idx = index_[k.index].index;
    assert(data_idx < rindex_.size());
    assert(rindex_[data_idx].index == k.index);
    if (data_idx != data_.size() - 1) {
        data_[data_idx] = std::move(data_.back());
        rindex_[data_idx] = rindex_.back();
        index_[rindex_[data_idx].index].index = data_idx;
    }
    data_.pop_back();
    rindex_.pop_back();
    index_[k.index].index = first_free_;
    first_free_ = k.index;
}

template <typename T>
bool store<T>::has_key(key k) const
{
    if (k.index >= index_.size()) { return false; }
    auto data_idx = index_[k.index].index;
    if (data_idx >= rindex_.size()) { return false; }
    return rindex_[data_idx] == k;
}
#endif // STORE_STORE_HPP
