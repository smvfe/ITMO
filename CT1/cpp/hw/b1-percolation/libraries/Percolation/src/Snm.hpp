#ifndef SNM_H
#define SNM_H

#include <vector>

class Snm {
public:
    Snm(std::size_t n);

    int find(int p);

    void unite(int p, int q);

    bool is_united(int p, int q);

private:
    std::vector<int> parent;
    std::vector<int> size;
};

#endif  // SNM_H