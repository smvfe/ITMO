#include "tree/Tree.hpp"

class TreapImpl {
public:
    int key;
    unsigned int priority;
    TreapImpl* left;
    TreapImpl* right;

    TreapImpl(int k, unsigned int p) : key(k), priority(p), left(nullptr), right(nullptr) {}
    ~TreapImpl() {
        delete left;
        delete right;
    }
};

int generate_priority() {
    thread_local static std::mt19937 gen(std::random_device{}());
    thread_local static std::uniform_int_distribution dist(1, std::numeric_limits<int>::max());
    return dist(gen);
}

TreapImpl* found_boundary(TreapImpl* root, int value, bool (*predicate)(int, int)) {
    TreapImpl* result  = nullptr;
    TreapImpl* current = root;

    while (current) {
        if (predicate(current->key, value)) {
            result  = current;
            current = current->left;
        } else {
            current = current->right;
        }
    }

    return result;
}

TreapImpl* find_prev(TreapImpl* root, int value) {
    return found_boundary(root, value, [](int node_key, int val) { return node_key >= val; });
}

TreapImpl* find_next(TreapImpl* root, int value) {
    return found_boundary(root, value, [](int node_key, int val) { return node_key > val; });
}

TreapImpl* find_min(TreapImpl* root) {
    if (!root)
        return nullptr;
    while (root->left)
        root = root->left;
    return root;
}

TreapImpl* find_max(TreapImpl* root) {
    if (!root)
        return nullptr;
    while (root->right)
        root = root->right;
    return root;
}

std::pair<TreapImpl*, TreapImpl*> split(TreapImpl* tree, const int key) {
    if (!tree)
        return {nullptr, nullptr};

    if (key > tree->key) {
        auto [left_subtree, right_subtree] = split(tree->right, key);
        tree->right                        = left_subtree;
        return {tree, right_subtree};
    } else {
        auto [left_subtree, right_subtree] = split(tree->left, key);
        tree->left                         = right_subtree;
        return {left_subtree, tree};
    }
}

TreapImpl* merge(TreapImpl* left_tree, TreapImpl* right_tree) {
    if (!left_tree)
        return right_tree;
    if (!right_tree)
        return left_tree;

    TreapImpl* result_root;
    if (left_tree->priority > right_tree->priority) {
        left_tree->right = merge(left_tree->right, right_tree);
        result_root      = left_tree;
    } else {
        right_tree->left = merge(left_tree, right_tree->left);
        result_root      = right_tree;
    }

    return result_root;
}

std::vector<int> inorder_traversal(TreapImpl* root) {
    std::vector<int> result;

    std::vector<TreapImpl*> stack;
    TreapImpl* current = root;

    while (current || !stack.empty()) {
        while (current) {
            stack.push_back(current);
            current = current->left;
        }

        current = stack.back();
        stack.pop_back();

        result.push_back(current->key);
        current = current->right;
    }

    return result;
}

TreapImpl* clone_tree(TreapImpl* node) {
    if (!node)
        return nullptr;

    TreapImpl* new_node = new TreapImpl(node->key, node->priority);
    new_node->left      = clone_tree(node->left);
    new_node->right     = clone_tree(node->right);
    return new_node;
}

Treap::Treap() : root(nullptr), tree_size(0) {}

Treap::Treap(const Treap& other) : root(nullptr), tree_size(0) {
    if (this != &other && other.root) {
        root      = clone_tree(other.root);
        tree_size = other.tree_size;
    }
}

Treap::~Treap() {
    delete root;
}

int Treap::top() const {
    return root->key;
}

bool Treap::contains(int value) const noexcept {
    TreapImpl* current = root;
    while (current) {
        if (value == current->key)
            return true;
        current = (value < current->key) ? current->left : current->right;
    }
    return false;
}

bool Treap::insert(int value) {
    if (contains(value))
        return false;

    unsigned int new_priority = generate_priority();

    if (!root) {
        root      = new TreapImpl(value, new_priority);
        tree_size = 1;
        return true;
    }

    TreapImpl* current      = root;
    TreapImpl** current_ptr = &root;
    while (current && current->priority >= new_priority) {
        current_ptr = (value < current->key) ? &(current->left) : &(current->right);
        current     = *current_ptr;
    }

    TreapImpl* new_node = new TreapImpl(value, new_priority);
    if (!current) {
        *current_ptr = new_node;
    } else {
        auto [left_tree, right_tree] = split(current, value);
        new_node->left               = left_tree;
        new_node->right              = right_tree;
        *current_ptr                 = new_node;
    }

    tree_size++;
    return true;
}

bool Treap::remove(int value) {
    if (!root)
        return false;

    if (root->key == value) {
        TreapImpl* old_root = root;
        root                = merge(root->left, root->right);

        old_root->left = old_root->right = nullptr;
        delete old_root;
        tree_size--;
        return true;
    }

    TreapImpl* parent  = nullptr;
    TreapImpl* current = root;
    bool is_left_child = false;
    while (current && current->key != value) {
        parent = current;
        if (value < current->key) {
            current       = current->left;
            is_left_child = true;
        } else {
            current       = current->right;
            is_left_child = false;
        }
    }

    if (!current) {
        return false;
    }

    TreapImpl* merged = merge(current->left, current->right);
    if (is_left_child) {
        parent->left = merged;
    } else {
        parent->right = merged;
    }

    current->left = current->right = nullptr;
    delete current;
    tree_size--;

    return true;
}

std::size_t Treap::size() const noexcept {
    return tree_size;
}

bool Treap::empty() const noexcept {
    return root == nullptr;
}

std::vector<int> Treap::values() const noexcept {
    if (!root)
        return {};
    return inorder_traversal(root);
}