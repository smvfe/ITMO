#include "gtest/gtest.h"
#include "libtesttask/libtesttask.hpp"

TEST(sum, loop) {
    for (std::uint64_t i{0}; i < 1000; ++i) {
        for (std::uint64_t j{0}; j < 1000; ++j) {
            EXPECT_EQ(i + j, sumFunc(i, j));
        }
    }
}
