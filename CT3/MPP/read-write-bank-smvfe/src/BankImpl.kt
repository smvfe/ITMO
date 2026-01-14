import java.util.concurrent.locks.ReentrantReadWriteLock
import kotlin.concurrent.withLock

/**
 * Bank implementation.
 *
 * @author Mochekov Semyon
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val accountsCount: Int
        get() = accounts.size

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun amount(id: Int): Long {
        val account = accounts[id]
        account.lock.readLock().withLock {
            return account.amount
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override val totalAmount: Long
        get() {
            for (account in accounts) {
                account.lock.readLock().lock()
            }
            try {
                return accounts.sumOf { account ->
                    account.amount
                }
            } finally {
                for (account in accounts) {
                    account.lock.readLock().unlock()
                }
            }
        }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun deposit(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        account.lock.writeLock().withLock {
            check(amount <= Bank.MAX_AMOUNT && account.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
            account.amount += amount
            return account.amount
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun withdraw(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        account.lock.writeLock().withLock {
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun transfer(fromId: Int, toId: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromId != toId) { "fromId == toId" }
        val from = accounts[fromId]
        val to = accounts[toId]

        val first_id = minOf(fromId, toId)
        val second_id = maxOf(fromId, toId)

        accounts[first_id].lock.writeLock().withLock {
            accounts[second_id].lock.writeLock().withLock {
                check(amount <= from.amount) { "Underflow" }
                check(amount <= Bank.MAX_AMOUNT && to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
                from.amount -= amount
                to.amount += amount
            }
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun consolidate(fromIds: List<Int>, toId: Int) {
        require(fromIds.isNotEmpty()) { "empty fromIds" }
        require(fromIds.distinct() == fromIds) { "duplicates in fromIds" }
        require(toId !in fromIds) { "toId in fromIds" }

        val all_ids = (fromIds + toId).sorted()
        val locks = all_ids.map { accounts[it].lock.writeLock() }

        locks.forEach { it.lock() }
        try {
            val from_list = fromIds.map { accounts[it] }
            val to = accounts[toId]
            val amount = from_list.sumOf { it.amount }
            check(to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
            for (from in from_list) from.amount = 0
            to.amount += amount
        } finally {
            locks.forEach { it.unlock() }
        }
    }

    /**
     * Private account data structure.
     */
    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0
        val lock = ReentrantReadWriteLock()
    }
}