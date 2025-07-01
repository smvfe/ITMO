#include "acp/Pool.hpp"

PoolAllocator::PoolAllocator(const std::size_t minP, const std::size_t maxP) {}

void* PoolAllocator::allocate(std::size_t const) {
    throw std::bad_alloc{};
    //    return std::malloc(sz);
}

void PoolAllocator::deallocate(void const*) {
    //    std::free(const_cast<void*>(ptr));
}
