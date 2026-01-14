@file:Suppress("DuplicatedCode", "FoldInitializerAndIfToElvis")

import java.util.concurrent.atomic.*

/**
 * Implementation of the Michael-Scott queue algorithm with constant-time removal.
 *
 * @author Mochekov Semyon
 *
 * TODO: Use your MichaelScottQueue implementation as a starting point.
 * TODO: Read instructions in the code.
 */
class MSQueueWithConstantTimeRemove<E> : QueueWithRemove<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(element = null, prev = null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        while (true) {
            val cur_tail = tail.get()
            val new_node = Node(element = element, prev = cur_tail)
            if (cur_tail.next.compareAndSet(null, new_node)) {
                tail.compareAndSet(cur_tail, new_node)
                if (cur_tail.extractedOrRemoved) {
                    cur_tail.physicallyRemove()
                }
                return
            } else {
                tail.compareAndSet(cur_tail, cur_tail.next.get()!!)
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val cur_head = head.get()
            val cur_head_next = cur_head.next.get() ?: return null
            if (head.compareAndSet(cur_head, cur_head_next)) {
                cur_head_next.prev.set(null)
                if (cur_head_next.markExtractedOrRemoved()) {
                    return cur_head_next.element
                } else {
                    continue
                }
            }
        }
    }

    override fun remove(element: E): Boolean {
        // Traverse the linked list, searching the specified
        // element. Try to remove the corresponding node if found.
        // DO NOT CHANGE THIS CODE.
        var node = head.get()
        while (true) {
            val next = node.next.get()
            if (next == null) return false
            node = next
            if (node.element == element && node.remove()) return true
        }
    }

    /**
     * This is an internal function for tests.
     * DO NOT CHANGE THIS CODE.
     */
    override fun validate() {
        check(head.get().prev.get() == null) {
            "`head.prev` must be null"
        }
        check(tail.get().next.get() == null) {
            "tail.next must be null"
        }
        // Traverse the linked list
        var node = head.get()
        while (true) {
            if (node !== head.get() && node !== tail.get()) {
                check(!node.extractedOrRemoved) {
                    "Removed node with element ${node.element} found in the middle of the queue"
                }
            }
            val nodeNext = node.next.get()
            // Is this the end of the linked list?
            if (nodeNext == null) break
            // Is next.prev points to the current node?
            val nodeNextPrev = nodeNext.prev.get()
            check(nodeNextPrev != null) {
                "The `prev` pointer of node with element ${nodeNext.element} is `null`, while the node is in the middle of the queue"
            }
            check(nodeNextPrev == node) {
                "node.next.prev != node; `node` contains ${node.element}, `node.next` contains ${nodeNext.element}"
            }
            // Process the next node.
            node = nodeNext
        }
    }

    private class Node<E>(
        var element: E?,
        prev: Node<E>?
    ) {
        val next = AtomicReference<Node<E>?>(null)
        val prev = AtomicReference(prev)

        /**
         * TODO: Both [dequeue] and [remove] should mark
         * TODO: nodes as "extracted or removed".
         */
        private val _extractedOrRemoved = AtomicBoolean(false)
        val extractedOrRemoved
            get() =
                _extractedOrRemoved.get()

        fun markExtractedOrRemoved(): Boolean =
            _extractedOrRemoved.compareAndSet(false, true)

        /**
         * Removes this node from the queue structure.
         * Returns `true` if this node was successfully
         * removed, or `false` if it has already been
         * removed by [remove] or extracted by [dequeue].
         */
        fun remove(): Boolean {
            if (!markExtractedOrRemoved()) {
                return false
            }
            physicallyRemove()
            return true
        }
        
        fun physicallyRemove() {
            while (true) {
                val prev_node = prev.get() ?: return
                val next_node = next.get() ?: return
                if (prev.get() == null) return
                next_node.prev.compareAndSet(this, prev_node)
                prev_node.next.compareAndSet(this, next_node)
                if (prev_node.extractedOrRemoved) {
                    prev_node.physicallyRemove()
                }
                if (next_node.extractedOrRemoved) {
                    next_node.physicallyRemove()
                }
                return
            }
        }
    }
}