#ifndef ACP_POOL_HPP
#define ACP_POOL_HPP

#include <cstddef>
#include <initializer_list>
#include <new>

class PoolAllocator {
public:
    PoolAllocator(std::size_t const minP, std::size_t const maxP);

    void* allocate(std::size_t const /*n*/);

    void deallocate(void const* /*ptr*/);
};

#endif  // ACP_POOL_HPP
