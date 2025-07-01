#include <algorithm>
#include <array>
#include <cstddef>
#include <iomanip>
#include <memory>
#include <new>
#include <numeric>
#include <optional>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "gtest/gtest.h"

#ifdef JEMALLOC
#include "jemalloc/jemalloc.h"
#endif

#include "acp/Pool.hpp"

namespace {

template <std::size_t Size>
struct Dummy {
    unsigned char data[Size];

    Dummy() {
        for (std::size_t i = 0; i < Size; ++i) {
            data[i] = 0x5a;
        }
    }

    ~Dummy() {
        for (std::size_t i = 0; i < Size; ++i) {
            data[i] = 0xa5;
        }
    }

    bool check() const {
        for (std::size_t i = 0; i < Size; ++i) {
            if (data[i] != 0x5a) {
                return false;
            }
        }
        return true;
    }
};

struct Complex {
    int a;
    char &b;
    const double c;

    Complex(const int a_, char &b_, const double c_) : a(a_), b(b_), c(c_) {}

    ~Complex() { a = -1; }
};

template <std::size_t Size, std::size_t Count>
struct Params {
    static constexpr std::size_t size  = Size;
    static constexpr std::size_t count = Count;
};

constexpr unsigned upper_bin_power(const std::size_t n) {
    unsigned i = 0;
    while ((static_cast<std::size_t>(1) << i) < n) {
        ++i;
    }
    return i;
}

constexpr std::size_t get_capacity(const unsigned min_power, const unsigned max_power, const std::size_t target_size) {
    unsigned target_power = upper_bin_power(target_size);
    if (target_power < min_power) {
        target_power = min_power;
    }
    if (target_power > max_power) {
        return 0;
    }
    return static_cast<std::size_t>(1) << (max_power - target_power);
}

constexpr std::size_t get_block_count(const unsigned min_power, const std::size_t target_size) {
    unsigned target_power = upper_bin_power(target_size);
    if (target_power <= min_power) {
        return 1;
    }
    return static_cast<std::size_t>(1) << (target_power - min_power);
}

template <class P>
struct AllocatorTest: ::testing::Test {
    static constexpr std::size_t dummy_size = P::size * P::count;
    static constexpr unsigned max_power = std::max(upper_bin_power(sizeof(Complex)) - 1, upper_bin_power(dummy_size));
    static constexpr unsigned min_power = max_power > 6 ? (max_power - 6) : 1;
    static constexpr std::size_t block_count      = static_cast<std::size_t>(1) << (max_power - min_power);
    static constexpr std::size_t dummy_capacity   = get_capacity(min_power, max_power, P::size);
    static constexpr std::size_t complex_capacity = get_capacity(min_power, max_power, sizeof(Complex));
    static constexpr std::size_t dummy_cost       = get_block_count(min_power, P::size);
    static constexpr std::size_t complex_cost     = get_block_count(min_power, sizeof(Complex));

    PoolAllocator alloc;

    template <class T, class... Args>
    T *create(Args &&...args) {
        auto aptr = alloc.allocate(sizeof(T));
        new (aptr) T(std::forward<Args>(args)...);
        return static_cast<T *>(aptr);
    }

    template <class T>
    void destroy(const T *ptr) {
        if (ptr) {
            ptr->~T();
            alloc.deallocate(ptr);
        }
    }

    AllocatorTest() : alloc(min_power, max_power) {}

    using D = Dummy<P::size>;

    D *create_dummy() { return create<D>(); }

    void destroy_dummy(const D *ptr) { destroy(ptr); }

    Complex *create_complex(const int a, char &b, const double c) { return create<Complex>(a, b, c); }

    void destroy_complex(const Complex *ptr) { destroy(ptr); }
};

using TestedTypes = ::testing::Types<Params<1, 1>, Params<1, 24>, Params<3, 1>, Params<7, 4>, Params<7, 41>,
                                     Params<10, 20>, Params<256, 1>, Params<256, 256>>;
TYPED_TEST_SUITE(AllocatorTest, TestedTypes);

}  // anonymous namespace

TEST(BasicAllocatorTest, allocate_smallest) {
    PoolAllocator alloc(0, 2);
    void *first  = alloc.allocate(2);
    void *second = alloc.allocate(1);
    alloc.deallocate(first);
    void *third  = alloc.allocate(1);
    void *fourth = alloc.allocate(2);
    alloc.deallocate(second);
    alloc.deallocate(third);
    alloc.deallocate(fourth);
}

TYPED_TEST(AllocatorTest, single_dummy) {
    auto ptr     = this->create_dummy();
    ptr->data[0] = 112;
    this->destroy_dummy(ptr);
}

TYPED_TEST(AllocatorTest, single_complex) {
    char x = '@';
    if (this->complex_capacity > 0) {
        auto ptr = this->create_complex(-511, x, 0.05);
        EXPECT_EQ(-511, ptr->a);
        EXPECT_EQ('@', ptr->b);
        EXPECT_DOUBLE_EQ(0.05, ptr->c);
        this->destroy_complex(ptr);
    } else {
        EXPECT_THROW(this->create_complex(0, x, 0.01), std::bad_alloc);
    }
}

TYPED_TEST(AllocatorTest, full_dummy) {
    using Ptr = decltype(this->create_dummy());
    std::vector<Ptr> ptrs;
    for (std::size_t i = 0; i < this->dummy_capacity; ++i) {
        auto ptr     = this->create_dummy();
        ptr->data[0] = 199;
        ptrs.push_back(ptr);
    }
    EXPECT_THROW(this->create_dummy(), std::bad_alloc);
    EXPECT_THROW(this->create_dummy(), std::bad_alloc);
    for (auto ptr : ptrs) {
        EXPECT_EQ(199, ptr->data[0]);
        this->destroy_dummy(ptr);
    }
    EXPECT_NO_THROW(this->destroy_dummy(this->create_dummy()));
}

TYPED_TEST(AllocatorTest, full_complex) {
    using Ptr = decltype(this->create_complex(1, std::declval<char &>(), 0.1));
    std::vector<Ptr> ptrs;
    char x         = 'X';
    int n          = -11;
    const double d = 1.11e-3;
    for (std::size_t i = 0; i < this->complex_capacity; ++i, --n) {
        auto ptr = this->create_complex(n, x, d);
        EXPECT_EQ(n, ptr->a);
        EXPECT_EQ(x, ptr->b);
        EXPECT_DOUBLE_EQ(d, ptr->c);
        ptrs.push_back(ptr);
    }
    if (this->complex_capacity > 0) {
        EXPECT_THROW(this->create_complex(0, x, 0.01), std::bad_alloc);
    }
    n = -11;
    for (auto ptr : ptrs) {
        EXPECT_EQ(n, ptr->a);
        EXPECT_EQ(x, ptr->b);
        EXPECT_DOUBLE_EQ(d, ptr->c);
        --n;
        this->destroy_complex(ptr);
    }
    if (this->complex_capacity > 0) {
        EXPECT_NO_THROW(this->destroy_complex(this->create_complex(0, x, 0.01)));
    }
}

TYPED_TEST(AllocatorTest, full_mixed) {
    using DPtr = decltype(this->create_dummy());
    using CPtr = decltype(this->create_complex(1, std::declval<char &>(), 0.1));
    std::vector<DPtr> d_ptrs;
    std::vector<CPtr> c_ptrs;
    char x                = '7';
    const double d        = 0x1.4p3;
    const int n           = -113;
    const unsigned char u = 0x1F;
    std::size_t available = this->block_count;
    d_ptrs.reserve(available);
    c_ptrs.reserve(available);
    while (available >= this->complex_cost || available >= this->dummy_cost) {
        if (available >= this->complex_cost) {
            c_ptrs.push_back(this->create_complex(n, x, d));
            EXPECT_DOUBLE_EQ(d, c_ptrs.back()->c) << "Bad complex element";
            available -= this->complex_cost;
        }
        if (available >= this->dummy_cost) {
            d_ptrs.push_back(this->create_dummy());
            d_ptrs.back()->data[0] = u;
            available -= this->dummy_cost;
        }
    }
    EXPECT_TRUE(available < this->dummy_cost && available < this->complex_cost);
    EXPECT_THROW(this->create_dummy(), std::bad_alloc);
    if (this->complex_capacity > 0) {
        EXPECT_THROW(this->create_complex(0, x, 0.01), std::bad_alloc);
    }
    std::size_t i = 0;
    for (auto ptr : c_ptrs) {
        EXPECT_EQ(n, ptr->a) << "Bad " << i << " complex element";
        EXPECT_EQ(x, ptr->b) << "Bad " << i << " complex element";
        EXPECT_DOUBLE_EQ(d, ptr->c) << "Bad " << i << " complex element";
        this->destroy_complex(ptr);
        ++i;
    }
    i = 0;
    for (auto ptr : d_ptrs) {
        EXPECT_EQ(u, ptr->data[0]) << "Bad " << i << " dummy element";
        this->destroy_dummy(ptr);
        ++i;
    }
}

TYPED_TEST(AllocatorTest, dummy_fragmentation) {
    using DPtr = decltype(this->create_dummy());
    using CPtr = decltype(this->create_complex(1, std::declval<char &>(), 0.1));
    std::vector<DPtr> d_ptrs;
    for (std::size_t i = 0; i < this->dummy_capacity; ++i) {
        d_ptrs.push_back(this->create_dummy());
    }

    std::size_t freed = 0;
    for (std::size_t i = 0; i < d_ptrs.size(); i += 2) {
        this->destroy_dummy(d_ptrs[i]);
        d_ptrs[i] = nullptr;
        ++freed;
    }

    char x         = ' ';
    const double d = 0xF.Fp10;
    int n          = 0;
    std::vector<CPtr> c_ptrs;
    if (this->dummy_cost < this->complex_cost) {
        EXPECT_THROW(this->create_complex(n, x, d), std::bad_alloc);
    } else {
        for (std::size_t i = 0; i < freed; ++i) {
            EXPECT_NO_THROW(c_ptrs.push_back(this->create_complex(n, x, d)))
                << "Failed to create " << i << " out of " << freed << " complex element";
            ++n;
        }
    }

    n = 0;
    for (auto ptr : d_ptrs) {
        if (ptr) {
            EXPECT_TRUE(ptr->check()) << "Bad " << n << " dummy element";
        }
        this->destroy_dummy(ptr);
        ++n;
    }
    n = 0;
    for (auto ptr : c_ptrs) {
        EXPECT_EQ(n, ptr->a) << "Bad " << n << " complex element";
        EXPECT_EQ(x, ptr->b) << "Bad " << n << " complex element";
        EXPECT_DOUBLE_EQ(d, ptr->c) << "Bad " << n << " complex element";
        this->destroy_complex(ptr);
        ++n;
    }
}

TYPED_TEST(AllocatorTest, dummy_defragmentation) {
    using DPtr = decltype(this->create_dummy());
    using CPtr = decltype(this->create_complex(1, std::declval<char &>(), 0.1));
    std::vector<DPtr> d_ptrs;
    for (std::size_t i = 0; i < this->dummy_capacity; ++i) {
        d_ptrs.push_back(this->create_dummy());
    }

    char x         = ' ';
    const double d = 0xF.Fp10;
    EXPECT_THROW(this->create_complex(0, x, d), std::bad_alloc);

    std::size_t dummy_available = 0;
    // for defragmentation to happen it's required
    // to free blocks in power of 2 quantities and they have
    // to occupy the same subtree;
    // we start with the first allocated block (it should be in the leftmost subtree)
    // and go by 4 dummy objects: free 4, skip 4 and so on
    const std::size_t free_batch       = 4;
    const std::size_t complex_in_batch = (this->dummy_cost * free_batch) / this->complex_cost;
    const std::size_t dummy_leftover =
        (this->dummy_cost * free_batch - complex_in_batch * this->complex_cost) / this->dummy_cost;
    std::size_t freed = 0;
    for (std::size_t i = 0; i < d_ptrs.size(); i += free_batch) {
        if ((i + free_batch) < d_ptrs.size()) {
            for (std::size_t j = 0; j < free_batch; ++i, ++j) {
                this->destroy_dummy(d_ptrs[i]);
                d_ptrs[i] = nullptr;
            }
            freed += complex_in_batch;
            dummy_available += dummy_leftover;
        }
    }

    int n = 0;
    std::vector<CPtr> c_ptrs;
    c_ptrs.reserve(freed);
    for (std::size_t i = 0; i < freed; ++i) {
        EXPECT_NO_THROW(c_ptrs.push_back(this->create_complex(n, x, d)))
            << "Failed to create " << i << " out of " << freed << " complex element";
        ++n;
    }
    EXPECT_THROW(this->create_complex(n, x, d), std::bad_alloc);
    for (std::size_t i = 0; i < dummy_available; ++i) {
        d_ptrs.push_back(this->create_dummy());
    }
    EXPECT_THROW(this->create_dummy(), std::bad_alloc);

    n = 0;
    for (auto ptr : d_ptrs) {
        if (ptr) {
            EXPECT_TRUE(ptr->check()) << "Bad " << n << " dummy element";
        }
        this->destroy_dummy(ptr);
        ++n;
    }
    n = 0;
    for (auto ptr : c_ptrs) {
        EXPECT_EQ(n, ptr->a) << "Bad " << n << " complex element";
        EXPECT_EQ(x, ptr->b) << "Bad " << n << " complex element";
        EXPECT_DOUBLE_EQ(d, ptr->c) << "Bad " << n << " complex element";
        this->destroy_complex(ptr);
        ++n;
    }
}

TYPED_TEST(AllocatorTest, complex_fragmentation) {
    using DPtr     = decltype(this->create_dummy());
    using CPtr     = decltype(this->create_complex(1, std::declval<char &>(), 0.1));
    char x         = ' ';
    const double d = 0xC.EFFp4;
    int n          = 0;
    std::vector<CPtr> c_ptrs;
    std::vector<DPtr> d_ptrs;
    for (std::size_t i = 0; i < this->complex_capacity; ++i) {
        c_ptrs.push_back(this->create_complex(n, x, d));
        ++n;
    }
    const std::size_t used = this->complex_capacity * this->complex_cost;
    if (this->block_count > used + this->dummy_cost) {
        const std::size_t leftover = (this->block_count - used) / this->dummy_cost;
        for (std::size_t i = 0; i < leftover; ++i) {
            d_ptrs.push_back(this->create_dummy());
        }
    } else {
        EXPECT_THROW(this->create_dummy(), std::bad_alloc);
    }

    std::size_t freed = 0;
    for (std::size_t i = 0; i < c_ptrs.size(); i += 2) {
        this->destroy_complex(c_ptrs[i]);
        c_ptrs[i] = nullptr;
        ++freed;
    }
    if (this->dummy_cost <= this->complex_cost) {
        for (std::size_t i = 0; i < freed; ++i) {
            d_ptrs.push_back(this->create_dummy());
        }
    } else {
        EXPECT_THROW(this->create_dummy(), std::bad_alloc);
    }

    n = 0;
    for (auto ptr : c_ptrs) {
        if (ptr) {
            EXPECT_EQ(n, ptr->a);
            EXPECT_EQ(x, ptr->b);
            EXPECT_DOUBLE_EQ(d, ptr->c);
        }
        this->destroy_complex(ptr);
        ++n;
    }
    for (auto ptr : d_ptrs) {
        EXPECT_TRUE(ptr->check());
        this->destroy_dummy(ptr);
    }
}

#ifdef JEMALLOC
namespace {

std::size_t get_current_size() {
    std::size_t sz = 0;
    EXPECT_EQ(0, mallctl("thread.tcache.flush", nullptr, nullptr, nullptr, 0));
    std::uint64_t epoch = 1;
    sz                  = sizeof(epoch);
    EXPECT_EQ(0, mallctl("epoch", &epoch, &sz, &epoch, sz));
    std::size_t allocated = 0;
    sz                    = sizeof(allocated);
    EXPECT_EQ(0, mallctl("stats.allocated", &allocated, &sz, nullptr, 0));
    return allocated;
}

}  // anonymous namespace

TEST(MemoryRequirements, check) {
    const auto before = get_current_size();
    {
        PoolAllocator alloc(4, 12);  // min block is 16 bytes, total size is 4096 bytes
        const std::size_t capacity = 4096 / 16;
        for (std::size_t i = 0; i < capacity; ++i) {
            alloc.allocate(3);
        }
        const auto created            = get_current_size();
        const std::size_t block_count = 2 * 4096 / 16 - 1;
        const double per_block        = (created - before) / (block_count * 1.0);
        EXPECT_GE(sizeof(void *) * 7, per_block);
    }
}

#endif
