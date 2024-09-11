#include <columnist/multivector.hpp>

#include <catch2/catch_test_macros.hpp>

using columnist::multivector;

TEST_CASE("a default constructed multivector is empty")
{
    multivector<int, char, double> v;
    REQUIRE(v.empty());
    REQUIRE(v.size() == 0);
}

TEST_CASE(
    "when objects are pushed_back, the vector grows and the elements stored")
{
    multivector<int, std::unique_ptr<int>, double, char> v;
    v.push_back(1, std::make_unique<int>(3), 3.14, 'c');
    v.push_back(2, std::make_unique<int>(5), 1.5, 'd');
    REQUIRE(v.size() == 2);
    REQUIRE(v.capacity() == 2);
    v.push_back(3, std::make_unique<int>(8), 2.25, 'e');
    REQUIRE(v.size() == 3);
    REQUIRE(v.capacity() == 4);
}

struct throwing_counted {
    struct error {};

    throwing_counted(int v)
    : value(v)
    {
        if (v < 0) { throw error{}; }
        ++objects;
        ++constructed;
    }

    throwing_counted(const throwing_counted& v) noexcept
    : value(v.value)
    {
        ++objects;
        ++copy_constructed;
    }

    throwing_counted(throwing_counted&& v) noexcept
    : value(v.value)
    {
        v.value = -1;
        ++objects;
        ++move_constructed;
    }

    ~throwing_counted()
    {
        --objects;
        ++destroyed;
    };

    throwing_counted& operator=(const throwing_counted& v)
    {
        value = v.value;
        return *this;
    }

    throwing_counted& operator=(throwing_counted&& v) noexcept
    {
        value = v.value;
        v.value = -1;
        return *this;
    }

    int value;
    static inline size_t objects = 0;
    static inline size_t constructed = 0;
    static inline size_t copy_constructed = 0;
    static inline size_t move_constructed = 0;
    static inline size_t destroyed = 0;
};

TEST_CASE("when a pushed back element throws, those previously constructed for "
          "the same row are destroyed")
{
    GIVEN("a multivector of types that may throw")
    {
        multivector<throwing_counted, throwing_counted, throwing_counted> v;
        v.reserve(2);
        v.push_back(1, 2, 3);
        WHEN("a constructor throws in push_back")
        {
            auto objects_before = throwing_counted::objects;
            auto constructed_before = throwing_counted::constructed;
            auto destroyed_before = throwing_counted::destroyed;
            REQUIRE_THROWS(v.push_back(1, 2, -3));
            THEN("the vector size stays the same, but the capacity may have "
                 "increased")
            {
                REQUIRE(v.size() == 1);
                REQUIRE(v.capacity() == 2);
            }
            AND_THEN("the objects that were constructed in the push_back are "
                     "destroyed")
            {
                REQUIRE(throwing_counted::objects == objects_before);
                REQUIRE(throwing_counted::constructed
                        == constructed_before + 2);
                REQUIRE(throwing_counted::destroyed == destroyed_before + 2);
            }
        }
    }
}

TEST_CASE("indexing")
{
    multivector<int, char, double> v;
    v.push_back(1, 'c', 3.14);
    v.push_back(2, 'd', 1.5);
    v.push_back(3, 'e', 2.5);
    REQUIRE(v[0] == std::tuple(1, 'c', 3.14));
    REQUIRE(v[1] == std::tuple(2, 'd', 1.5));
    REQUIRE(v[2] == std::tuple(3, 'e', 2.5));

    auto& nc = std::get<0>(v[0]);
    STATIC_REQUIRE(std::is_same_v<decltype(nc), int&>);
    auto& c = std::get<0>(std::as_const(v)[0]);
    STATIC_REQUIRE(std::is_same_v<decltype(c), const int&>);
}
