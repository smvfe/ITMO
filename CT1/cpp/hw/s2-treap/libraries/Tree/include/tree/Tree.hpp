#ifndef TREE_HPP
#define TREE_HPP

#include <random>
#include <vector>

class TreapImpl;

class Treap {
public:
    [[nodiscard]] bool contains(int value) const noexcept;
    bool insert(int value);
    bool remove(int value);

    [[nodiscard]] std::size_t size() const noexcept;
    [[nodiscard]] bool empty() const noexcept;
    [[nodiscard]] std::vector<int> values() const noexcept;

    Treap();
    Treap(const Treap& other);
    ~Treap();

private:
    TreapImpl* root;
    std::size_t tree_size;

    int top() const;
};

#endif