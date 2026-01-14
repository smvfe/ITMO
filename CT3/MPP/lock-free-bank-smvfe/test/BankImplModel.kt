/**
 * Bank implementation. Single-threaded version.
 */
class BankImplModel(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val accountsCount: Int
        get() = accounts.size

    override fun amount(id: Int): Long {
        return accounts[id].amount
    }

    override val totalAmount: Long
        get() {
            return accounts.sumOf { account ->
                account.amount
            }
        }

    override fun deposit(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        check(amount <= MAX_AMOUNT && account.amount + amount <= MAX_AMOUNT) { "Overflow" }
        account.amount += amount
        return account.amount
    }

    override fun withdraw(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        check(account.amount - amount >= 0) { "Underflow" }
        account.amount -= amount
        return account.amount
    }

    override fun transfer(fromId: Int, toId: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromId != toId) { "fromId == toId" }
        val from = accounts[fromId]
        val to = accounts[toId]
        check(amount <= from.amount) { "Underflow" }
        check(amount <= MAX_AMOUNT && to.amount + amount <= MAX_AMOUNT) { "Overflow" }
        from.amount -= amount
        to.amount += amount
    }

    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0
    }
}