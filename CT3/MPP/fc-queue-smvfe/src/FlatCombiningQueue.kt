import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author TODO: Mochekov Semyon
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with the element. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        if (combinerLock.compareAndSet(false, true)) {
            try {
                queue.addLast(element)
                help()
            } finally {
                combinerLock.set(false)
            }
            return
        }

        val idx = pubOp(element)
        while (true) {
            val cell = tasksForCombiner.get(idx)
            if (cell is Result<*>) {
                tasksForCombiner.set(idx, null)
                return
            }
            if (combinerLock.compareAndSet(false, true)) {
                try {
                    help()
                    val res = tasksForCombiner.get(idx)
                    if (res is Result<*>) {
                        tasksForCombiner.set(idx, null)
                        return
                    }
                } finally {
                    combinerLock.set(false)
                }
            }
            Thread.onSpinWait()
        }
    }

    override fun dequeue(): E? {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with `Dequeue`. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        if (combinerLock.compareAndSet(false, true)) {
            try {
                val res = queue.removeFirstOrNull()
                help()
                return res
            } finally {
                combinerLock.set(false)
            }
        }

        val idx = pubOp(Dequeue)
        while (true) {
            val cell = tasksForCombiner.get(idx)
            if (cell is Result<*>) {
                tasksForCombiner.set(idx, null)
                @Suppress("UNCHECKED_CAST")
                return cell.value as E?
            }
            if (combinerLock.compareAndSet(false, true)) {
                try {
                    help()
                    val res = tasksForCombiner.get(idx)
                    if (res is Result<*>) {
                        tasksForCombiner.set(idx, null)
                        @Suppress("UNCHECKED_CAST")
                        return res.value as E?
                    }
                } finally {
                    combinerLock.set(false)
                }
            }
            Thread.onSpinWait()
        }
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())

    private fun help() {
        val len = tasksForCombiner.length()
        for (i in 0 until len) {
            val op = tasksForCombiner.get(i)
            when (op) {
                null -> {}
                is Result<*> -> {}
                Dequeue -> {
                    val value = queue.removeFirstOrNull()
                    tasksForCombiner.set(i, Result(value))
                }
                else -> {
                    @Suppress("UNCHECKED_CAST")
                    val elem = op as E
                    queue.addLast(elem)
                    tasksForCombiner.set(i, Result(Unit))
                }
            }
        }
    }

    private fun pubOp(op: Any?): Int {
        val len = tasksForCombiner.length()
        var start = randomCellIndex()
        var attempts = 0
        while (attempts < len * 2) {
            val idx = (start + attempts) % len
            if (tasksForCombiner.compareAndSet(idx, null, op)) return idx
            attempts++
        }
        while (true) {
            val idx = randomCellIndex()
            if (tasksForCombiner.compareAndSet(idx, null, op)) return idx
            Thread.onSpinWait()
        }
    }
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

// TODO: Put this token in `tasksForCombiner` for dequeue().
// TODO: enqueue()-s should put the inserting element.
private object Dequeue

// TODO: Put the result wrapped with `Result` when the operation in `tasksForCombiner` is processed.
private class Result<V>(
    val value: V
)
