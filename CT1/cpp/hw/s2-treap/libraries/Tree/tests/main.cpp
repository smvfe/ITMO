#include "tree/Tree.hpp"
using Tree = TASK_VARIANT;

#include <algorithm>
#include <random>

#include "gtest/gtest.h"

namespace {
std::string get_double_insert_error_message(int value) {
    return "inserting an already contained value, the return value must be false; invalid insert for value = " +
           std::to_string(value);
}

std::string get_double_remove_error_message(int value) {
    return "inserting not contained value, the return value must be false; invalid remove for value = " +
           std::to_string(value);
}

int get_random_number() {
    thread_local static std::mt19937 random(std::random_device{}());
    return static_cast<int>(2 * 1e9 * std::uniform_real_distribution{}(random));
}

}  // namespace

/**
 * General tests
 */
class TreeTest: public ::testing::Test {};

TEST_F(TreeTest, contains_in_empty) {
    Tree tree;
    ASSERT_FALSE(tree.contains(1));
}

TEST_F(TreeTest, insert) {
    Tree tree;
    ASSERT_TRUE(tree.insert(1));
    ASSERT_TRUE(tree.contains(1));
}

TEST_F(TreeTest, multiple_insert) {
    Tree tree;
    int value = 1;

    ASSERT_TRUE(tree.insert(value));
    ASSERT_TRUE(tree.contains(value));

    ASSERT_FALSE(tree.insert(value)) << get_double_insert_error_message(value);
    ASSERT_TRUE(tree.contains(value));

    ASSERT_FALSE(tree.insert(value)) << get_double_insert_error_message(value);
}

TEST_F(TreeTest, insert_and_check_order) {
    Tree tree;
    std::vector<int> values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    std::shuffle(values.begin(), values.end(), std::mt19937{std::random_device{}()});

    for (auto value : values) {
        ASSERT_TRUE(tree.insert(value));
        ASSERT_FALSE(tree.insert(value)) << get_double_insert_error_message(value);
    }

    for (auto value : values) {
        ASSERT_TRUE(tree.contains(value)) << "tree not contains " << value;
    }

    std::sort(values.begin(), values.end());

    ASSERT_EQ(values, tree.values());
}

TEST_F(TreeTest, remove) {
    Tree tree;

    ASSERT_TRUE(tree.insert(1));
    ASSERT_TRUE(tree.contains(1));

    ASSERT_TRUE(tree.remove(1));
    ASSERT_FALSE(tree.contains(1));
}

TEST_F(TreeTest, remove_from_empty_tree) {
    Tree tree;
    ASSERT_FALSE(tree.remove(1));
}

TEST_F(TreeTest, multiple_remove) {
    Tree tree;
    int value = 1;

    ASSERT_TRUE(tree.insert(value));

    ASSERT_TRUE(tree.remove(value));
    ASSERT_FALSE(tree.remove(value)) << get_double_remove_error_message(1);
}

TEST_F(TreeTest, remove_and_check_order) {
    Tree tree;
    std::vector<int> values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    std::shuffle(values.begin(), values.end(), std::mt19937{std::random_device{}()});

    for (auto value : values) {
        ASSERT_TRUE(tree.insert(value));
    }

    std::shuffle(values.begin(), values.end(), std::mt19937{std::random_device{}()});

    std::vector<int> values_to_removing;
    for (std::size_t i = 0; i < values.size() / 2; ++i) {
        values_to_removing.push_back(values[i]);
    }

    std::vector<int> remaining_values;
    for (std::size_t i = values.size() / 2; i < values.size(); ++i) {
        remaining_values.push_back(values[i]);
    }
    std::sort(remaining_values.begin(), remaining_values.end());

    for (auto value : values_to_removing) {
        ASSERT_TRUE(tree.remove(value)) << "Value = " << value;
        ASSERT_FALSE(tree.remove(value)) << get_double_remove_error_message(value);
    }

    ASSERT_EQ(tree.size(), values.size() - values.size() / 2);

    for (auto value : values_to_removing) {
        ASSERT_FALSE(tree.contains(value)) << "Value = " << value;
    }

    for (auto value : remaining_values) {
        ASSERT_TRUE(tree.contains(value)) << "Value = " << value;
    }

    ASSERT_EQ(remaining_values, tree.values());
}

/**
 * Performance testing on large input data
 */
class PerformanceTest: public ::testing::TestWithParam<int> {};

TEST_P(PerformanceTest, insertion_in_ascending_order) {
    Tree tree;
    int count_of_values = GetParam();
    for (int i = 0; i < count_of_values; ++i) {
        ASSERT_TRUE(tree.insert(i));
    }
}

TEST_P(PerformanceTest, insertion_random_values) {
    int count_of_values = GetParam();
    std::set<int> values;
    for (int i = 0; i < 2 * count_of_values; ++i) {
        values.insert(::get_random_number());
    }

    Tree tree;
    std::for_each(values.cbegin(), values.cend(), [&tree](int value) { ASSERT_TRUE(tree.insert(value)); });

    std::for_each(values.cbegin(), values.cend(),
                  [&tree](int value) { ASSERT_TRUE(tree.contains(value)) << "Value = " << value; });

    ASSERT_EQ(tree.size(), values.size());
}

TEST_P(PerformanceTest, insertion_and_removing_random_values) {
    int count_of_values = GetParam();
    std::set<int> values;
    for (int i = 0; i < 2 * count_of_values; ++i) {
        values.insert(::get_random_number());
    }

    Tree tree;
    std::for_each(values.cbegin(), values.cend(), [&tree](int value) { ASSERT_TRUE(tree.insert(value)); });

    std::for_each(values.cbegin(), values.cend(), [&tree](int value) { ASSERT_TRUE(tree.contains(value)); });

    ASSERT_EQ(tree.size(), values.size());

    std::for_each(values.cbegin(), values.cend(), [&tree](int value) { ASSERT_TRUE(tree.remove(value)); });

    ASSERT_TRUE(tree.empty());
}

INSTANTIATE_TEST_SUITE_P(TreeTest, PerformanceTest, ::testing::Values(1e3, 1e4, 2 * 1e5));

int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
