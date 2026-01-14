import java.util.concurrent.atomic.*

/**
 * @author TODO: Мочеков Семён
 *
 * TODO: Copy the code from `FAABasedQueueSimplified`
 * TODO: and implement the infinite array on a linked list
 * TODO: of fixed-size `Segment`s.
 */
class FAABasedQueue<E> : Queue<E> {
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)
    private val head = AtomicReference(Segment(0))
    private val tail = AtomicReference(head.get())

    override fun enqueue(element: E) {
        while (true) {
            val i = enqIdx.getAndIncrement()
            val segId = i / SEGMENT_SIZE
            val off = (i % SEGMENT_SIZE).toInt()
            val seg = getSegment(segId)
            if (seg.cells.compareAndSet(off, null, element)) return
        }
    }

    override fun dequeue(): E? {
        val base = deqIdx.getAndIncrement()
        var idx = base
        while (true) {
            val segId = idx / SEGMENT_SIZE
            val off = (idx % SEGMENT_SIZE).toInt()
            val seg = getSegment(segId)
            val cur = seg.cells.get(off)
            when {
                cur == null -> {
                    if (seg.cells.compareAndSet(off, null, POISONED)) {
                        if (enqIdx.get() <= idx) return null
                        idx++
                    } else {}
                }
                cur === POISONED -> {
                    idx++
                }
                else -> {
                    if (seg.cells.compareAndSet(off, cur, POISONED)) {
                        @Suppress("UNCHECKED_CAST")
                        return cur as E
                    }
                }
            }
        }
    }

    private fun getSegment(targetId: Long): Segment {
        while (true) {
            var cur = head.get()
            while (cur.id < targetId) {
                var next = cur.next.get()
                if (next == null) {
                    val newSeg = Segment(cur.id + 1)
                    if (cur.next.compareAndSet(null, newSeg)) {
                        next = newSeg
                    } else {
                        next = cur.next.get()
                    }
                }

                if (next != null) {
                    val t = tail.get()
                    if (t.id < next.id) tail.compareAndSet(t, next)
                }
                cur = next ?: continue
            }
            if (cur.id == targetId) return cur
        }
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2
private val POISONED = Any()
