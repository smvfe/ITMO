import kotlin.concurrent.atomics.AtomicReference
import kotlin.concurrent.atomics.ExperimentalAtomicApi

/*
   Obstruction-free STM implementation.
   @author : Mochekov Semyon
*/

/**
 * Atomic block.
 */
fun <T> atomic(block: TxScope.() -> T): T {
    while (true) {
        val transaction = Transaction()
        try {
            val result = block(transaction)
            if (transaction.commit()) return result
            transaction.abort()
        } catch (e: AbortException) {
            transaction.abort()
        }
    }
}

/**
 * Transactional operations are performed in this scope.
 */
abstract class TxScope {
    abstract fun <T> TxVar<T>.read(): T
    abstract fun <T> TxVar<T>.write(x: T): T
}

/**
 * Transactional variable.
 */
@OptIn(ExperimentalAtomicApi::class)
class TxVar<T>(initial: T)  {
    private val loc = AtomicReference(Loc(initial, initial, rootTx))

    /**
     * Opens this transactional variable in the specified transaction [tx] and applies
     * updating function [update] to it. Returns the updated value.
     */
    fun openIn(tx: Transaction, update: (T) -> T): T {
        while (true) {
            if (tx.status != TxStatus.ACTIVE) throw AbortException
            
            val cur_lc = loc.load()
            val ownr = cur_lc.ownr
            
            if (ownr === tx) {
                val upd_val = update(cur_lc.newValue)
                val nxt_loc = Loc(cur_lc.oldValue, upd_val, tx)
                if (loc.compareAndSet(cur_lc, nxt_loc)) return upd_val
                continue
            }
            
            val ownr_status = ownr.status
            
            val cur_val = when (ownr_status) {
                TxStatus.COMMITTED -> cur_lc.newValue
                TxStatus.ABORTED -> cur_lc.oldValue
                TxStatus.ACTIVE -> {
                    ownr.abort()
                    continue
                }
            }
            
            val upd_val = update(cur_val)
            val nxt_loc = Loc(cur_val, upd_val, tx)
            if (loc.compareAndSet(cur_lc, nxt_loc)) return upd_val
        }
    }
}

/**
 * State of transactional value
 */
private class Loc<T>(
    val oldValue: T,
    val newValue: T,
    val ownr: Transaction
)

private val rootTx = Transaction().apply { commit() }

/**
 * Transaction status.
 */
enum class TxStatus { ACTIVE, COMMITTED, ABORTED }

/**
 * Transaction implementation.
 */
@OptIn(ExperimentalAtomicApi::class)
class Transaction : TxScope() {
    private val _status = AtomicReference(TxStatus.ACTIVE)
    val status: TxStatus get() = _status.load()

    fun commit(): Boolean =
        _status.compareAndSet(TxStatus.ACTIVE, TxStatus.COMMITTED)

    fun abort() {
        _status.compareAndSet(TxStatus.ACTIVE, TxStatus.ABORTED)
    }

    override fun <T> TxVar<T>.read(): T = openIn(this@Transaction) { it }
    override fun <T> TxVar<T>.write(x: T) = openIn(this@Transaction) { x }
}

/**
 * This exception is thrown when transaction is aborted.
 */
private object AbortException : Exception() {
    override fun fillInStackTrace(): Throwable = this
}