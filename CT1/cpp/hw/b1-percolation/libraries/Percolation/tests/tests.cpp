#include "Percolation.hpp"
#include "PercolationStats.hpp"
#include "gtest/gtest.h"

TEST(PercolationStatsTests, Percolation10Trials1000) {
    const size_t dimension = 10;
    const size_t trials    = 1000;
    PercolationStats stats(dimension, trials);

    const double precision = 0.01;
    EXPECT_NEAR(0.5909, stats.mean(), precision);
    EXPECT_NEAR(0.0740, stats.standardDeviation(), precision);
    EXPECT_NEAR(0.5893, stats.confidenceLow(), precision);
    EXPECT_NEAR(0.5975, stats.confidenceHigh(), precision);
}

TEST(PercolationStatsTests, Percolation5Trials5000) {
    const size_t dimension = 5;
    const size_t trials    = 5000;
    PercolationStats stats(dimension, trials);

    const double precision = 0.01;
    EXPECT_NEAR(0.5929, stats.mean(), precision);
    EXPECT_NEAR(0.1030, stats.standardDeviation(), precision);
    EXPECT_NEAR(0.5908, stats.confidenceLow(), precision);
    EXPECT_NEAR(0.5949, stats.confidenceHigh(), precision);
}

TEST(PercolationStatsTests, Percolation2Trials10k) {
    const size_t dimension = 2;
    const size_t trials    = 10'000;
    PercolationStats stats(dimension, trials);

    const double precision = 0.01;
    EXPECT_NEAR(0.6669, stats.mean(), precision);
    EXPECT_NEAR(0.1177, stats.standardDeviation(), precision);
    EXPECT_NEAR(0.6662, stats.confidenceLow(), precision);
    EXPECT_NEAR(0.6676, stats.confidenceHigh(), precision);
}

TEST(PercolationStatsTests, Percolation1Trials1000) {
    // if dimension == 1, than mean() is always 1
    const size_t dimension = 1;
    const size_t trials    = 1000;
    PercolationStats stats(dimension, trials);

    const double precision = 0.01;
    EXPECT_NEAR(1, stats.mean(), precision);
    EXPECT_NEAR(0, stats.standardDeviation(), precision);
    EXPECT_NEAR(1, stats.confidenceLow(), precision);
    EXPECT_NEAR(1, stats.confidenceHigh(), precision);
}

TEST(PercolationTest, Percolation3x3) {
    // 0 x x x
    // 1 x x x
    // 2 x x x
    const size_t dimension = 3;
    Percolation percolation(dimension);
    ASSERT_FALSE(percolation.isOpen(0, 0));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 o x x
    // 1 x x x
    // 2 x x x
    percolation.open(0, 0);
    ASSERT_TRUE(percolation.isOpen(0, 0));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 o x x
    // 1 o x x
    // 2 x x x
    percolation.open(1, 0);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 o x x
    // 1 o x x
    // 2 x o x
    percolation.open(2, 1);
    ASSERT_TRUE(percolation.isOpen(2, 1));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 o x x
    // 1 o o x
    // 2 x o x
    percolation.open(1, 1);
    ASSERT_TRUE(percolation.isOpen(1, 1));
    ASSERT_TRUE(percolation.hasPercolation());
    ASSERT_EQ(4, percolation.numberOfOpenCells());
}

TEST(PercolationTest, PercolationIsFullCheck) {
    // check if isFull(size_t row, size_t column) works

    // 0 x x x
    // 1 x x x
    // 2 x x x
    const size_t dimension = 3;
    Percolation percolation(dimension);
    ASSERT_FALSE(percolation.isOpen(1, 0));
    ASSERT_FALSE(percolation.isFull(1, 0));
    ASSERT_FALSE(percolation.isOpen(1, 2));
    ASSERT_FALSE(percolation.isFull(1, 2));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 x x x
    // 1 o x x
    // 2 x x x
    percolation.open(1, 0);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_FALSE(percolation.isFull(1, 0));
    ASSERT_FALSE(percolation.isOpen(1, 2));
    ASSERT_FALSE(percolation.isFull(1, 2));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 x x x
    // 1 o x o
    // 2 x x x
    percolation.open(1, 2);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_FALSE(percolation.isFull(1, 0));
    ASSERT_TRUE(percolation.isOpen(1, 2));
    ASSERT_FALSE(percolation.isFull(1, 2));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 x o x
    // 1 o x o
    // 2 x x x
    percolation.open(0, 1);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_FALSE(percolation.isFull(1, 0));
    ASSERT_TRUE(percolation.isOpen(1, 2));
    ASSERT_FALSE(percolation.isFull(1, 2));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 x o x
    // 1 o o o
    // 2 x x x
    percolation.open(1, 1);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_TRUE(percolation.isFull(1, 0));
    ASSERT_TRUE(percolation.isOpen(1, 2));
    ASSERT_TRUE(percolation.isFull(1, 2));
    ASSERT_FALSE(percolation.hasPercolation());

    // 0 x o x
    // 1 o o o
    // 2 x o x
    percolation.open(2, 1);
    ASSERT_TRUE(percolation.isOpen(1, 0));
    ASSERT_TRUE(percolation.isFull(1, 0));
    ASSERT_TRUE(percolation.isOpen(1, 2));
    ASSERT_TRUE(percolation.isFull(1, 2));
    ASSERT_TRUE(percolation.hasPercolation());
}

TEST(PercolationTest, Percolation7x7SpiralTest) {
    // check if "spiral filling" works

    // 0 x x x x x x x
    // 1 x x x x x x x
    // 2 x x x x x x x
    // 3 x x x x x x x
    // 4 x x x x x x x
    // 5 x x x x x x x
    // 6 x x x x x x x
    const size_t dimension = 7;
    Percolation percolation(dimension);

    // 0 x x x x x x x
    // 1 o o o o o x o
    // 2 o x x x o x o
    // 3 o x o o o x o
    // 4 o x x x x x o
    // 5 o o o o o o o
    // 6 x x x x x x x
    for (int x = 1; x < 6; x++) {
        percolation.open(x, 0);
        percolation.open(x, 6);
    }
    for (int x = 1; x < 6; x += 4) {
        percolation.open(x, 1);
    }
    for (int x = 1; x < 6; x += 2) {
        percolation.open(x, 2);
        percolation.open(x, 3);
    }
    for (int x = 1; x < 4; x++) {
        percolation.open(x, 4);
    }
    percolation.open(5, 4);
    percolation.open(5, 5);
    ASSERT_FALSE(percolation.isFull(3, 2));

    // 0 x x x x x x o
    // 1 o o o o o x o
    // 2 o x x x o x o
    // 3 o x o o o x o
    // 4 o x x x x x o
    // 5 o o o o o o o
    // 6 x x x x x x x
    percolation.open(0, 6);
    ASSERT_TRUE(percolation.isFull(3, 2));
}

TEST(PercolationTest, Percolation5x5) {
    // hard test 5x5

    // 0 x x x x x
    // 1 x x x x x
    // 2 x x x x x
    // 3 x x x x x
    // 4 x x x x x
    const size_t dimension = 5;
    Percolation percolation(dimension);

    // 0 x x x x o
    // 1 o o o x o
    // 2 o x o x o
    // 3 o x o o o
    // 4 o x x x x
    for (int x = 1; x < 5; x++) {
        percolation.open(x, 0);
    }
    percolation.open(1, 1);
    for (int x = 1; x < 4; x++) {
        percolation.open(x, 2);
    }
    percolation.open(3, 3);
    for (int x = 0; x < 4; x++) {
        percolation.open(x, 4);
    }
    ASSERT_TRUE(percolation.hasPercolation());
}

TEST(PercolationTest, PercolationGetkNumbetCheck) {
    // check if open() works correct on already opened cell
    // check if numberOfOpenCells() works correct on full-open table
    const size_t dimension = 3;
    Percolation percolation(dimension);

    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            const int col = i * 3 + j + 1;
            percolation.open(i, j);
            ASSERT_EQ(col, percolation.numberOfOpenCells());
            percolation.open(i, j);
            ASSERT_EQ(col, percolation.numberOfOpenCells());
        }
    }
    ASSERT_EQ(9, percolation.numberOfOpenCells());
    percolation.open(0, 0);
    ASSERT_EQ(9, percolation.numberOfOpenCells());
}

int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
