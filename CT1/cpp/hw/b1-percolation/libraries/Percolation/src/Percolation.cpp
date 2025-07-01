#include "Percolation.hpp"

#include <vector>

#include "Snm.hpp"

std::size_t index(std::size_t n, std::size_t row, std::size_t column) {
    return row * n + column;
}

struct PercolationImpl {
    std::size_t n;
    std::vector<bool> grid;
    Snm snm;
    Snm snm_filled;
    std::size_t opened_counter;
    std::size_t virtual_top;
    std::size_t virtual_bottom;

    PercolationImpl(std::size_t dimension)
        : n(dimension)
        , grid(n * n, false)
        , snm(n * n + 2)
        , snm_filled(n * n + 1)
        , opened_counter(0)
        , virtual_top(n * n)
        , virtual_bottom(n * n + 1) {}
};

Percolation::Percolation(std::size_t dimension) : percl_imp(new PercolationImpl(dimension)) {}

Percolation::~Percolation() {
    delete percl_imp;
}

void Percolation::open(std::size_t row, std::size_t column) {
    std::size_t idx = index(percl_imp->n, row, column);
    if (!percl_imp->grid[idx]) {
        percl_imp->grid[idx] = true;
        percl_imp->opened_counter++;

        if (row > 0 && isOpen(row - 1, column)) {
            percl_imp->snm.unite(idx, index(percl_imp->n, row - 1, column));
            percl_imp->snm_filled.unite(idx, index(percl_imp->n, row - 1, column));
        }
        if (row < percl_imp->n - 1 && isOpen(row + 1, column)) {
            percl_imp->snm.unite(idx, index(percl_imp->n, row + 1, column));
            percl_imp->snm_filled.unite(idx, index(percl_imp->n, row + 1, column));
        }
        if (column > 0 && isOpen(row, column - 1)) {
            percl_imp->snm.unite(idx, index(percl_imp->n, row, column - 1));
            percl_imp->snm_filled.unite(idx, index(percl_imp->n, row, column - 1));
        }
        if (column < percl_imp->n - 1 && isOpen(row, column + 1)) {
            percl_imp->snm.unite(idx, index(percl_imp->n, row, column + 1));
            percl_imp->snm_filled.unite(idx, index(percl_imp->n, row, column + 1));
        }

        if (row == 0) {
            percl_imp->snm.unite(idx, percl_imp->virtual_top);
            percl_imp->snm_filled.unite(idx, percl_imp->virtual_top);
        }
        if (row == percl_imp->n - 1) {
            percl_imp->snm.unite(idx, percl_imp->virtual_bottom);
        }
    }
}

bool Percolation::isOpen(std::size_t row, std::size_t column) const {
    return percl_imp->grid[index(percl_imp->n, row, column)];
}

bool Percolation::isFull(std::size_t row, std::size_t column) const {
    std::size_t idx = index(percl_imp->n, row, column);
    if (!percl_imp->grid[idx]) {
        return false;
    }
    return percl_imp->snm_filled.is_united(idx, percl_imp->virtual_top);
}

std::size_t Percolation::numberOfOpenCells() const {
    return percl_imp->opened_counter;
}

bool Percolation::hasPercolation() const {
    return percl_imp->snm.is_united(percl_imp->virtual_top, percl_imp->virtual_bottom);
}