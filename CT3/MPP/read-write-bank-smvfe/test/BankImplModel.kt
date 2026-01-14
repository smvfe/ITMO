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
        check(amount <= Bank.MAX_AMOUNT && account.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
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
        check(amount <= Bank.MAX_AMOUNT && to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
        from.amount -= amount
        to.amount += amount
    }

    override fun consolidate(fromIds: List<Int>, toId: Int) {
        require(fromIds.isNotEmpty()) { "empty fromIds" }
        require(fromIds.distinct() == fromIds) { "duplicates in fromIds" }
        require(toId !in fromIds) { "toId in fromIds" }
        val fromList = fromIds.map { accounts[it] }
        val to = accounts[toId]
        val amount = fromList.sumOf { it.amount }
        check(to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
        for (from in fromList) from.amount = 0
        to.amount += amount
    }

    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0
    }
}