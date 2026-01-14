import java.util.concurrent.atomic.*
import kotlin.math.*

/**
 * @author TODO: Мочеков Семён
 */
class FAABasedQueueSimplified<E> : Queue<E> {
    private val infiniteArray = AtomicReferenceArray<Any?>(1024) // conceptually infinite array
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    override fun enqueue(element: E) {
        while (true) {
            val i = enqIdx.getAndIncrement()
            val idx = i.toInt()
            if (infiniteArray.compareAndSet(idx, null, element)) return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        val base = deqIdx.getAndIncrement()
        var j = base.toInt()
        while (true) {
            val cur = infiniteArray.get(j)
            when {
                cur == null -> {
                    if (infiniteArray.compareAndSet(j, null, POISONED)) {
                        if (enqIdx.get() <= j.toLong()) return null
                        j++
                    } else {}
                }
                cur === POISONED -> {
                    j++
                }
                else -> {
                    if (infiniteArray.compareAndSet(j, cur, POISONED)) return cur as E
                }
            }
        }
    }

    override fun validate() {
        for (i in 0 until min(deqIdx.get().toInt(), enqIdx.get().toInt())) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `deqIdx = ${deqIdx.get()}` at the end of the execution"
            }
        }
        for (i in max(deqIdx.get().toInt(), enqIdx.get().toInt()) until infiniteArray.length()) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `enqIdx = ${enqIdx.get()}` at the end of the execution"
            }
        }
    }
}

// TODO: poison cells with this value.
private val POISONED = Any()
