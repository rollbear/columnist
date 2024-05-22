
#include <store/table.hpp>

#include <algorithm>
#include <cstring>
#include <deque>
#include <memory>
#include <optional>
#include <span>
#include <vector>

struct done {};

struct generator {
    generator(std::span<const uint8_t> d)
    : data(d)
    {}

    template <typename T = uint8_t>
    T get()
    {
        if (data.size() < sizeof(T)) { throw done{}; }
        T value;
        std::memcpy(&value, data.data(), sizeof(T));

        data = data.subspan(sizeof value);
        return value;
    }

    std::span<const uint8_t> data;
};

auto equals
    = [](auto t) { return [t](const auto& v) -> bool { return t == v; }; };

void fuzz(generator g)
{
    struct K {
        bool operator==(const K&) const = default;
        uint32_t idx: 24;
        uint32_t gen: 8;
    };

    using T = table<std::unique_ptr<K>>;
    std::optional<T> v;
    std::vector<T::key> keys;
    std::deque<T::key> retired_keys;

    try {
        size_t elems = 0;

        auto get_idx
            = [&keys, &g, &elems] { return g.get<unsigned>() % elems; };
        for (;;) {
            if (!v) { v.emplace(); }
            switch (g.get()) {
            case 0:
                v.reset();
                elems = 0;
                keys = {};
                retired_keys = {};
                break;
            case 1:
                assert(elems == v->size());
                break;
            case 2: {
                auto i = v->insert(std::make_unique<K>());
                auto k = std::get<0>(*i);
                assert(std::ranges::none_of(keys, equals(k)));
                assert(std::ranges::none_of(retired_keys, equals(k)));
                keys.push_back(k);
                ++elems;
                *std::get<0>((*v)[k]) = { k.index, k.generation };
                break;
            }
            case 3: {
                if (!v->empty()) {
                    auto idx = get_idx();
                    auto k = keys[idx];
                    assert(v->has_key(k));
                    v->erase(k);
                    --elems;
                    retired_keys.push_back(k);
                    if (retired_keys.size() > 250) { retired_keys.pop_front(); }
                    keys[idx] = std::move(keys.back());
                    keys.pop_back();
                }
                break;
            }
            case 4: {
                if (!v->empty()) {
                    auto idx = get_idx();
                    auto k = keys[idx];
                    assert(v->has_key(k));
                    auto& p = std::get<0>((*v)[k]);
                    assert(p->idx == k.index);
                    assert(p->gen == k.generation);
                }
                break;
            }
            case 5: {
                size_t count = 0;
                for (auto [k, val] : *v) {
                    assert(val->gen == k.generation);
                    assert(val->idx == k.index);
                    ++count;
                }
                assert(count == elems);
            }
            }
        }
    } catch (done) {
        return;
    }
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    fuzz(std::span{ data, size });
    return 0;
}
