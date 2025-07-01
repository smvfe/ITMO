#include "PercolationStats.hpp"

#include "Percolation.hpp"

PercolationStats::PercolationStats(std::size_t dimension, std::size_t trials)
    : n(dimension), trials_counter(trials), thresholds(trials), std_dev(0.0), gen(rd()), mean_value(0.0) {
    execute();
}

double PercolationStats::mean() const {
    return mean_value;
}

double PercolationStats::standardDeviation() const {
    return std_dev;
}

double PercolationStats::confidenceLow() const {
    return mean_value - 1.96 * std_dev / std::sqrt(trials_counter);
}

double PercolationStats::confidenceHigh() const {
    return mean_value + 1.96 * std_dev / std::sqrt(trials_counter);
}

void PercolationStats::execute() {
    for (std::size_t t = 0; t < trials_counter; t++) {
        Percolation percolation(n);
        std::vector<std::pair<std::size_t, std::size_t>> blocks;

        for (std::size_t i = 0; i < n; i++) {
            for (std::size_t j = 0; j < n; j++) {
                blocks.emplace_back(i, j);
            }
        }

        while (!percolation.hasPercolation() && !blocks.empty()) {
            std::uniform_int_distribution<std::size_t> dist(0, blocks.size() - 1);
            std::size_t rnd_index = dist(gen);

            std::size_t row    = blocks[rnd_index].first;
            std::size_t column = blocks[rnd_index].second;

            percolation.open(row, column);

            blocks[rnd_index] = blocks.back();
            blocks.pop_back();
        }

        thresholds[t] = static_cast<double>(percolation.numberOfOpenCells()) / (n * n);
    }

    for (const double threshold : thresholds) {
        mean_value += threshold;
    }
    mean_value /= trials_counter;

    if (trials_counter > 1) {
        for (const double threshold : thresholds) {
            std_dev += (threshold - mean_value) * (threshold - mean_value);
        }
        std_dev = std::sqrt(std_dev / (trials_counter - 1));
    }
}
