/**
 * Bank implementation. This class is implemented using Software Transaction Manager (STM).
 */
class BankImpl(override val accountsCount: Int) : Bank {
    private val accounts = Array(accountsCount) { TxVar(0L) }

    override fun amount(id: Int): Long = atomic {
        accounts[id].read()
    }

    override val totalAmount: Long get() = atomic {
        accounts.fold(0L) { s, a -> s + a.read() }
    }

    override fun deposit(id: Int, amount: Long): Long = atomic {
        require(amount > 0) { "Invalid amount: $amount" }
        val a = accounts[id]
        val update = a.read() + amount
        check(update <= MAX_AMOUNT) { "Overflow" }
        a.write(update)
    }

    override fun withdraw(id: Int, amount: Long): Long = atomic {
        require(amount > 0) { "Invalid amount: $amount" }
        val a = accounts[id]
        val update = a.read() - amount
        check(update >= 0) { "Underflow" }
        a.write(update)
    }

    override fun transfer(fromId: Int, toId: Int, amount: Long) = atomic<Unit> {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromId != toId) { "fromId == toId" }
        check(amount <= MAX_AMOUNT) { "Underflow/overflow" }
        val from = accounts[fromId]
        val to = accounts[toId]
        val fromUpdate = from.read() - amount
        val toUpdate = to.read() + amount
        check(toUpdate <= MAX_AMOUNT) { "Overflow" }
        check(fromUpdate >= 0) { "Underflow" }
        from.write(fromUpdate)
        to.write(toUpdate)
    }
}