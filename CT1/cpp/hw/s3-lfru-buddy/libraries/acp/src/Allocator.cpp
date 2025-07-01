#include "acp/Allocator.hpp"

AllocatorWithPool::AllocatorWithPool(const std::size_t minP, const std::size_t maxP) : PoolAllocator(minP, maxP) {}
