#include <columnist/multivector.hpp>

#include <tuple>
#include <vector>

#include <benchmark/benchmark.h>

static void grow_multivector(benchmark::State& state)
{
    const auto size = static_cast<size_t>(state.range(0));
    columnist::multivector<unsigned char, unsigned int, double> v;
    for (auto _ : state) {
        for (unsigned int i = 0; i != size; ++i) {
            benchmark::DoNotOptimize(
                get<0>(v.push_back((unsigned char)(i & 0xff), i, i)));
        }
    }
}

static void iterate_multivector(benchmark::State& state)
{
    const auto size = static_cast<size_t>(state.range(0));
    columnist::multivector<unsigned char, unsigned int, double> v;
    for (unsigned int i = 0; i != size; ++i) {
        v.push_back((unsigned char)(i & 0xff), i, i);
    }
    for (auto _ : state) {
        for (size_t i = 0; i != size; ++i) {
            auto [a, b, c] = v[i];
            benchmark::DoNotOptimize(a + b + c);
        }
    }
}

template <typename... Ts>
struct multivector {
    template <typename... Us>
    auto push_back(Us&&... us)
    {
        auto push = [&]<size_t... Is>(std::index_sequence<Is...>) {
            (std::get<Is>(v).push_back(std::forward<Us>(us)), ...);
            return std::forward_as_tuple(std::get<Is>(v).back()...);
        };
        return push(columns);
    }

    std::tuple<Ts&...> operator[](size_t idx)
    {
        return [&]<size_t... Is>(std::index_sequence<Is...>) {
            return std::forward_as_tuple(std::get<Is>(v)[idx]...);
        }(columns);
    }

    size_t size() const { return std::get<0>(v).size(); }

    bool empty() const { return size() == 0; }

    std::tuple<std::vector<Ts>...> v;
    static constexpr auto columns = std::index_sequence_for<Ts...>{};
};

static void grow_struct_of_vectors(benchmark::State& state)
{
    const auto size = static_cast<size_t>(state.range(0));
    multivector<unsigned char, unsigned int, double> v;
    for (auto _ : state) {
        // This code gets timed
        for (unsigned int i = 0; i != size; ++i) {
            benchmark::DoNotOptimize(
                get<0>(v.push_back((unsigned char)(i & 0xff), i, i)));
        }
    }
}

static void iterate_struct_of_vectors(benchmark::State& state)
{
    const auto size = static_cast<size_t>(state.range(0));
    multivector<unsigned char, unsigned int, double> v;
    for (unsigned int i = 0; i != size; ++i) {
        v.push_back((unsigned char)(i & 0xff), i, i);
    }
    for (auto _ : state) {
        for (size_t i = 0; i != size; ++i) {
            auto [a, b, c] = v[i];
            benchmark::DoNotOptimize(a + b + c);
        }
    }
}

// Register the function as a benchmark
BENCHMARK(grow_multivector)->RangeMultiplier(4)->Range(2U, 1U << 20);
BENCHMARK(iterate_multivector)->RangeMultiplier(4)->Range(2, 1 << 20);
BENCHMARK(grow_struct_of_vectors)->RangeMultiplier(4)->Range(2, 1 << 20);
BENCHMARK(iterate_struct_of_vectors)->RangeMultiplier(4)->Range(2, 1 << 20);

// Run the benchmark
BENCHMARK_MAIN();
