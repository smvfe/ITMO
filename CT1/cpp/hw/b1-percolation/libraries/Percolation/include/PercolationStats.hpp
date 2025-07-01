#ifndef PERCOLATIONSTATS_HPP
#define PERCOLATIONSTATS_HPP

#include <cstddef>
#include <cstdint>
#include <random>
#include <vector>

struct PercolationStats {
    /**
     * Construct a new Percolation Stats object
     * @param dimension dimension of percolation grid
     * @param trials amount of experiments
     */
    PercolationStats(std::size_t dimension, std::size_t trials);

    /**
     * Returns mean of percolation threshold (x¯ from description)
     */
    [[nodiscard]] double mean() const;

    /**
     * Returns standard deviation of percolation threshold (s from description)
     */
    [[nodiscard]] double standardDeviation() const;

    /**
     * Returns log edge of condidence interval
     */
    [[nodiscard]] double confidenceLow() const;

    /**
     * Returns high edge of confidence interval
     */
    [[nodiscard]] double confidenceHigh() const;

    /**
     * Makes all experiments, calculates statistic values
     */
    void execute();

private:
    std::size_t n;
    std::size_t trials_counter;
    std::vector<double> thresholds;
    std::random_device rd;
    double std_dev;
    std::mt19937 gen;
    double mean_value;
};

#endif  // PERCOLATIONSTATS_HPP
