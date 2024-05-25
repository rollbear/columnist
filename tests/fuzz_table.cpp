
#include <columnist/table.hpp>

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
    using columnist::table;

    struct K {
        bool operator==(const K&) const = default;

        uint32_t idx: 24;
        uint32_t gen: 8;
    };

    using T = table<std::unique_ptr<K>>;
    std::optional<T> v;
    std::vector<T::row_id> keys;
    std::deque<T::row_id> retired_keys;

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
                auto h = v->insert(std::make_unique<K>());
                assert(std::ranges::none_of(keys, equals(h)));
                assert(std::ranges::none_of(retired_keys, equals(h)));
                keys.push_back(h);
                ++elems;
                *get<0>((*v)[h]) = { h.index, h.generation };
                break;
            }
            case 3: {
                if (!v->empty()) {
                    auto idx = get_idx();
                    auto k = keys[idx];
                    assert(v->has_row_id(k));
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
                    assert(v->has_row_id(k));
                    auto& p = get<0>((*v)[k]);
                    assert(p->idx == k.index);
                    assert(p->gen == k.generation);
                }
                break;
            }
            case 5: {
                size_t count = 0;
                for (auto i = v->begin(); i != v->end(); ++i) {
                    auto k = (*i).row_id();
                    auto [val] = *i;
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
