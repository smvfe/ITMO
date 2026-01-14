import kotlin.concurrent.atomics.*

/**
 * AtomicArray with practical lock-free implementation of CAS and CAS2 operations
 * based on paper by Timothy L. Harris, Keir Fraser and Ian A. Pratt:
 * [A Practical Multi-Word Compare-and-Swap Operation](https://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf).
 *
 * @author TODO: Mochekov Semyon
 */
@Suppress("UNCHECKED_CAST")
@OptIn(ExperimentalAtomicApi::class)
class AtomicArray<E>(size: Int, initialValue: E) {
    private val a = atomicArrayOfNulls<Any>(size)

    init {
        for (i in 0 until size) a.storeAt(i, initialValue)
    }

    fun get(index: Int): E {
        while (true) {
            val t = a.loadAt(index)
            if (t is DCSSDescriptor<*>) {
                t.complete()
            } else if (t is CAS2Descriptor<*>) {
                t.complete()
            } else {
                return t as E
            }
        }
    }

    fun set(index: Int, value: E) {
        while (true) {
            val t = a.loadAt(index)
            if (t is DCSSDescriptor<*>) {
                t.complete()
            } else if (t is CAS2Descriptor<*>) {
                t.complete()
            } else if (a.compareAndSetAt(index, t, value)) {
                return
            }
        }
    }

    fun cas(index: Int, expected: E, update: E): Boolean {
        while (true) {
            val t = a.loadAt(index)
            if (t is DCSSDescriptor<*>) {
                t.complete()
            } else if (t is CAS2Descriptor<*>) {
                t.complete()
            } else if (t == expected) {
                if (a.compareAndSetAt(index, t, update)) return true
            } else {
                return false
            }
        }
    }

    fun cas2(index1: Int, expected1: E, update1: E,
             index2: Int, expected2: E, update2: E): Boolean {
        if (index1 == index2) {
            return if (expected1 == expected2) cas(index1, expected1, update2) else false
        }
        
        val descriptor = if (index1 < index2) {
            CAS2Descriptor(a, index1, expected1, update1, index2, expected2, update2)
        } else {
            CAS2Descriptor(a, index2, expected2, update2, index1, expected1, update1)
        }
        
        descriptor.apply()
        return descriptor.outcome.get() === Outcome.SUCCESS
    }

    private object Outcome {
        val UNDECIDED = Any()
        val SUCCESS = Any()
        val FAIL = Any()
    }

    private class DCSSDescriptor<E>(
        private val array: kotlin.concurrent.atomics.AtomicArray<Any?>,
        val parent: CAS2Descriptor<E>,
        val index: Int,
        val expected: E
    ) {
        fun complete() {
            val parentOutcome = parent.outcome.get()
            if (parentOutcome === Outcome.UNDECIDED) {
                array.compareAndSetAt(index, this, parent)
            } else {
                array.compareAndSetAt(index, this, expected)
            }
        }
    }

    private class CAS2Descriptor<E>(
        private val array: kotlin.concurrent.atomics.AtomicArray<Any?>,
        private val index1: Int,
        private val expected1: E,
        private val update1: E,
        private val index2: Int,
        private val expected2: E,
        private val update2: E
    ) {
        val outcome = java.util.concurrent.atomic.AtomicReference<Any>(Outcome.UNDECIDED)

        fun apply() {
            while (true) {
                val t1 = array.loadAt(index1)
                if (t1 is DCSSDescriptor<*>) {
                    t1.complete()
                } else if (t1 is CAS2Descriptor<*>) {
                    t1.complete()
                } else if (t1 !== expected1) {
                    outcome.compareAndSet(Outcome.UNDECIDED, Outcome.FAIL)
                    return
                } else if (array.compareAndSetAt(index1, t1, this)) {
                    break
                }
            }
            
            complete()
        }

        fun complete() {
            if (outcome.get() === Outcome.UNDECIDED) {
                val v2 = array.loadAt(index2)
                if (v2 === this) {
                    outcome.compareAndSet(Outcome.UNDECIDED, Outcome.SUCCESS)
                } else if (v2 is DCSSDescriptor<*>) {
                    v2.complete()
                    return complete()
                } else if (v2 is CAS2Descriptor<*>) {
                    v2.complete()
                    return complete()
                } else if (v2 !== expected2) {
                    outcome.compareAndSet(Outcome.UNDECIDED, Outcome.FAIL)
                } else {
                    val dcss = DCSSDescriptor(array, this, index2, expected2)
                    if (array.compareAndSetAt(index2, v2, dcss)) {
                        dcss.complete()
                    }
                    return complete()
                }
            }
            
            val success = outcome.get() === Outcome.SUCCESS
            if (success) {
                array.compareAndSetAt(index1, this, update1)
                array.compareAndSetAt(index2, this, update2)
            } else {
                array.compareAndSetAt(index1, this, expected1)
            }
        }
    }
}
