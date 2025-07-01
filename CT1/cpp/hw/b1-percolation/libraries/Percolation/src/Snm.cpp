#include "Snm.hpp"

Snm::Snm(std::size_t n) {
    parent.resize(n);
    size.resize(n, 1);
    for (std::size_t i = 0; i < n; i++) {
        parent[i] = static_cast<int>(i);
    }
};

int Snm::find(int p) {
    while (p != parent[p]) {
        parent[p] = parent[parent[p]];
        p         = parent[p];
    }
    return p;
}

void Snm::unite(int p, int q) {
    int root_p = find(p);
    int root_q = find(q);
    if (root_p == root_q)
        return;

    if (size[root_p] < size[root_q]) {
        parent[root_p] = root_q;
        size[root_q] += size[root_p];
    } else {
        parent[root_q] = root_p;
        size[root_p] += size[root_q];
    }
}

bool Snm::is_united(int p, int q) {
    return find(p) == find(q);
}