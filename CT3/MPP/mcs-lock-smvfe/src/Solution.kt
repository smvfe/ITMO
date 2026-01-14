import java.util.concurrent.atomic.*

class Solution(val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node?>(null)

    override fun lock(): Node {
        val cur = Node()
        cur.locked.value = true
        val pred = tail.getAndSet(cur)
        if (pred != null) {
            pred.next.value = cur
            while (cur.locked.value) {
                env.park()
            }
        }
        return cur
    }

    override fun unlock(node: Node) {
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.value == null) {}
            }
        }
        val next = node.next.value!!
        next.locked.value = false
        env.unpark(next.thread)
    }

    class Node {
        val thread = Thread.currentThread()
        val locked = AtomicReference(false)
        val next = AtomicReference<Node?>(null)
    }
}
