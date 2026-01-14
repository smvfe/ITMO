import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.*

/**
 * @author Мочеков, Семён
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()

    // TODO: Try to optimize concurrent push and pop operations,
    // TODO: synchronizing them in an `rendezvousSlots` cell.
    private val rendezvousSlots = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushWithElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushWithElimination(element: E): Boolean {
        // TODO: Choose a random cell in `rendezvousSlots`
        // TODO: and try to install the element there.
        // TODO: Wait `ELIMINATION_WAIT_CYCLES` loop cycles
        // TODO: in hope that a concurrent `pop()` grabs the
        // TODO: element. If so, clean the cell and finish,
        // TODO: returning `true`. Otherwise, move the cell
        // TODO: to the empty state and return `false`.

        val index = ThreadLocalRandom.current().nextInt(ELIMINATION_ARRAY_SIZE)

        if (!rendezvousSlots.compareAndSet(index, CELL_STATE_EMPTY, element)) {
            return false
        }

        repeat(ELIMINATION_WAIT_CYCLES) {
            if (rendezvousSlots.get(index) === CELL_STATE_RETRIEVED) {
                if (rendezvousSlots.compareAndSet(index, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)) {
                    return true
                }
            }
            Thread.onSpinWait()
        }

        while (true) {
            val value = rendezvousSlots.get(index)

            when (value) {
                CELL_STATE_RETRIEVED -> {
                    if (rendezvousSlots.compareAndSet(index, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)) {
                        return true
                    }
                    Thread.onSpinWait()
                }
                element -> {
                    if (rendezvousSlots.compareAndSet(index, element, CELL_STATE_EMPTY)) {
                        return false
                    }
                    Thread.onSpinWait()
                }
                else -> return false
            }
        }
    }

    override fun pop(): E? = tryPopWithElimination() ?: stack.pop()

    private fun tryPopWithElimination(): E? {
        // TODO: Choose a random cell in `rendezvousSlots`
        // TODO: and try to retrieve an element from there.
        // TODO: On success, return the element.
        // TODO: Otherwise, if the cell is empty, return `null`.
        val index = ThreadLocalRandom.current().nextInt(ELIMINATION_ARRAY_SIZE)
        val value = rendezvousSlots.get(index) ?: return null

        if (value === CELL_STATE_RETRIEVED) {
            return null
        }
        return if (rendezvousSlots.compareAndSet(index, value, CELL_STATE_RETRIEVED)) {
            @Suppress("UNCHECKED_CAST")
            value as E
        } else {
            null
        }
    }

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 3 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        // Initially, all cells are in EMPTY state.
        private val CELL_STATE_EMPTY = null

        // `tryPopElimination()` moves the cell state
        // to `RETRIEVED` if the cell contains an element.
        private val CELL_STATE_RETRIEVED = Any()
    }
}
