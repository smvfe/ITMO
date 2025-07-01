#include <iostream>

#include "Percolation.hpp"
#include "PercolationStats.hpp"

int main() {
    constexpr std::size_t dimension = 5;
    constexpr std::size_t trials    = 100;
    PercolationStats percolation_stats(dimension, trials);
    std::cout << "mean = " << percolation_stats.mean() << std::endl;

    return 0;
}
