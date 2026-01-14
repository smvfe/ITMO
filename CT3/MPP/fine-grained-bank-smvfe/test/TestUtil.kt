import kotlinx.atomicfu.locks.ReentrantLock
import java.util.concurrent.TimeUnit

val implClass = BankImpl::class.java
val accountClass = BankImpl.Account::class.java

fun configureLock(bank: Bank, lockFactory: (Int) -> ReentrantLock = { ReentrantLockWithTimeout() }) {
    val accounts =
        implClass.getDeclaredField("accounts").apply { isAccessible = true }.get(bank) as Array<*>
    val lockField = accountClass.getDeclaredField("lock").apply { isAccessible = true }
    for ((i, account) in accounts.withIndex()) {
        lockField.set(account, lockFactory(i))
    }
}

/**
 * A special implementation of [ReentrantLock] for testing purposes.
 */
open class ReentrantLockWithTimeout : ReentrantLock() {
    override fun lock() {
        if (!super.tryLock(3, TimeUnit.SECONDS)) {
            error("Lock timed out")
        }
    }
}
