import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicReference

/**
 * Int-to-Int hash map with open addressing and linear probes.
 * @author Mochekov Semyon
 */
class IntIntHashMap {
    private val core = AtomicReference(Core(INITIAL_CAPACITY))

    /**
     * Returns value for the corresponding key or zero if this key is not present.
     *
     * @param key a positive key.
     * @return value for the corresponding or zero if this key is not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    operator fun get(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(core.get().getInternal(key))
    }

    /**
     * Changes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key   a positive key.
     * @param value a positive value.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key or value are not positive, or value is equal to
     * [Integer.MAX_VALUE] which is reserved.
     */
    fun put(key: Int, value: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        require(isValue(value)) { "Invalid value: $value" }
        return toValue(putAndRehashWhileNeeded(key, value))
    }

    /**
     * Removes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key a positive key.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    fun remove(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(putAndRehashWhileNeeded(key, DEL_VALUE))
    }

    private fun putAndRehashWhileNeeded(key: Int, value: Int): Int {
        while (true) {
            val cur_core = core.get()
            val old_value = cur_core.putInternal(key, value)
            if (old_value != NEEDS_REHASH) return old_value
            core.compareAndSet(cur_core, cur_core.rehash())
        }
    }

    private class Core(capacity: Int) {
        // Pairs of <key, value> here, the actual
        // size of the map is twice as big.
        val map: AtomicIntegerArray = AtomicIntegerArray(2 * capacity)
        val shift: Int
        val next: AtomicReference<Core> = AtomicReference(null)

        init {
            val mask = capacity - 1
            assert(mask > 0 && mask and capacity == 0) { "Capacity must be power of 2: $capacity" }
            shift = 32 - Integer.bitCount(mask)
        }

        fun getInternal(key: Int): Int {
            var index = index(key)
            var probes = 0
            while (map.get(index) != key) {
                if (map.get(index) == NULL_KEY) return NULL_VALUE
                if (++probes >= MAX_PROBES) return NULL_VALUE
                if (index == 0) index = map.length()
                index -= 2
            }

            val value = map.get(index + 1)
            if (is_moved_value(value)) {
                val new_core = next.get()!!
                if (value != MOVED_NULL) {
                    new_core.copy_to_new_table(key, unmoved_value(value))
                }
                return new_core.getInternal(key)
            }
            return value
        }

        fun putInternal(key: Int, value: Int): Int {
            var index = index(key)
            var probes = 0
            while (true) {
                val cur_key = map.get(index)
                if (cur_key == key) break
                if (cur_key == NULL_KEY) {
                    if (value == DEL_VALUE) return NULL_VALUE
                    if (map.compareAndSet(index, NULL_KEY, key)) break
                    continue
                }
                if (++probes >= MAX_PROBES) return NEEDS_REHASH
                if (index == 0) index = map.length()
                index -= 2
            }

            while (true) {
                val old_value = map.get(index + 1)
                if (is_moved_value(old_value)) {
                    val new_core = next.get()!!
                    if (old_value != MOVED_NULL) {
                        new_core.copy_to_new_table(key, unmoved_value(old_value))
                    }
                    return new_core.putInternal(key, value)
                }
                if (map.compareAndSet(index + 1, old_value, value)) {
                    return old_value
                }
            }
        }

        fun rehash(): Core {
            next.compareAndSet(null, Core(map.length()))
            val new_core = next.get()!!
            var index = 0
            while (index < map.length()) {
                while (true) {
                    val cur_value = map.get(index + 1)
                    if (is_moved_value(cur_value)) {
                        if (cur_value != MOVED_NULL) {
                            val cur_key = map.get(index)
                            new_core.copy_to_new_table(cur_key, unmoved_value(cur_value))
                        }
                        break
                    }
                    
                    val moved_marker = if (cur_value == NULL_VALUE ||
                                           cur_value == DEL_VALUE) MOVED_NULL else moved_value(cur_value)
                    if (map.compareAndSet(index + 1, cur_value, moved_marker)) {
                        if (isValue(cur_value)) {
                            val cur_key = map.get(index)
                            new_core.copy_to_new_table(cur_key, cur_value)
                        }
                        break
                    }
                }
                index += 2
            }
            return new_core
        }
        
        fun copy_to_new_table(key: Int, value: Int) {
            var index = index(key)
            var probes = 0
            while (true) {
                val cur_key = map.get(index)
                if (cur_key == key) {
                    map.compareAndSet(index + 1, NULL_VALUE, value)
                    return
                }
                if (cur_key == NULL_KEY) {
                    if (map.compareAndSet(index, NULL_KEY, key)) {
                        map.compareAndSet(index + 1, NULL_VALUE, value)
                        return
                    }
                    continue
                }
                if (++probes >= MAX_PROBES) {
                    return
                }
                if (index == 0) index = map.length()
                index -= 2
            }
        }

        /**
         * Returns an initial index in map to look for a given key.
         */
        fun index(key: Int): Int = (key * MAGIC ushr shift) * 2
    }
}

private fun is_moved_value(value: Int): Boolean = value < 0 && value != NEEDS_REHASH
private fun moved_value(value: Int): Int = -(value + 1)
private fun unmoved_value(value: Int): Int = -(value + 1)
private const val MOVED_NULL = Int.MIN_VALUE

private const val MAGIC = -0x61c88647 // golden ratio
private const val INITIAL_CAPACITY = 2 // !!! DO NOT CHANGE INITIAL CAPACITY !!!
private const val MAX_PROBES = 8 // max number of probes to find an item
private const val NULL_KEY = 0 // missing key (initial value)
private const val NULL_VALUE = 0 // missing value (initial value)
private const val DEL_VALUE = Int.MAX_VALUE // mark for removed value
private const val NEEDS_REHASH = -1 // returned by `putInternal` to indicate that rehash is needed

// Checks is the value is in the range of allowed values
private fun isValue(value: Int): Boolean = value in (1 until DEL_VALUE)

// Converts internal value to the public results of the methods
private fun toValue(value: Int): Int = if (isValue(value)) value else 0