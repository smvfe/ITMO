import java.util.concurrent.atomic.*

/**
 * Implementation of the Michael-Scott queue algorithm.
 *
 * @author Мочеков Семён
 */
class MichaelScottQueue<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    fun enqueue(element: E) {
        val node = Node(element)

        while (true) {
            val cur_tail = tail.get()
            val nxt_tail = cur_tail.next.get()

            if (cur_tail === tail.get()) {
                if (nxt_tail == null) {
                    if (cur_tail.next.compareAndSet(null, node)) {
                        tail.compareAndSet(cur_tail, node)
                        return
                    }
                } else {
                    tail.compareAndSet(cur_tail, nxt_tail)
                }
            }
        }
    }

    fun dequeue(): E? {
        while (true) {
            val cur_head = head.get()
            val cur_tail = tail.get()
            val nxt_head = cur_head.next.get()

            if (cur_head === head.get()) {
                if (nxt_head == null) {
                    return null
                }

                if (cur_head === cur_tail) {
                    tail.compareAndSet(cur_tail, nxt_head)
                } else {
                    val value = nxt_head.element
                    
                    if (head.compareAndSet(cur_head, nxt_head)) {
                        nxt_head.element = null
                        return value
                    }
                }
            }
        }
    }

    // FOR TEST PURPOSE, DO NOT CHANGE IT.
    fun validate() {
        check(tail.get().next.get() == null) {
            "At the end of the execution, `tail.next` must be `null`"
        }
        check(head.get().element == null) {
            "At the end of the execution, the dummy node shouldn't store an element"
        }
    }

    private class Node<E>(
        var element: E?
    ) {
        val next = AtomicReference<Node<E>?>(null)
    }
}
