import java.util.concurrent.atomic.*

/**
 * @author Мочеков, Семён
 */
class TreiberStack<E> : Stack<E> {
    // Initially, the stack is empty.
    private val top = AtomicReference<Node<E>?>(null)

    override fun push(element: E) {
        while (true) {
            val cur_top = top.get()
            val node = Node(element, cur_top)
            if (top.compareAndSet(cur_top, node)) return
        }
    }

    override fun pop(): E? {
        while (true) {
            val cur_top = top.get() ?: return null
            val nxt_node = cur_top.next
            if (top.compareAndSet(cur_top, nxt_node)) {
                return cur_top.element
            }
        }
    }

    private class Node<E>(
        val element: E,
        val next: Node<E>?
    )
}
