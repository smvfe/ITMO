/**
 * Bank implementation.
 *
 * :TODO: This implementation has to be made thread-safe.
 *
 * @author Мочеков Семён
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
        account.lock.lock()

        try {
            return account.amount
        } finally {
            account.lock.unlock()
        }
    }

    override val totalAmount: Long
        get() {
            accounts.forEach { 
                it.lock.lock()
            }

            try {
                return accounts.sumOf {
                    it.amount
                }
            } finally {
                accounts.forEach {
                    it.lock.unlock()
                }
            }
        }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun deposit(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        account.lock.lock()

        try {
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) {
                "Overflow"
            }
            account.amount += amount
            return account.amount
        } finally {
            account.lock.unlock()
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun withdraw(id: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[id]
        account.lock.lock()
        
        try {
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        } finally {
            account.lock.unlock()
        }
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun transfer(fromId: Int, toId: Int, amount: Long) {
        require(amount > 0) {
            "Invalid amount: $amount"
        }
        require(fromId != toId) {
            "fromId == toId"
        }
        val fromAccount = accounts[fromId]
        val toAccount = accounts[toId]

        if (fromId < toId) {
            fromAccount.lock.lock()
            toAccount.lock.lock()
        } else {
            toAccount.lock.lock()
            fromAccount.lock.lock()
        }
        try {
            check(amount <= fromAccount.amount) {
                "Underflow"
            }
            check(!(amount > Bank.MAX_AMOUNT || toAccount.amount + amount > Bank.MAX_AMOUNT)) {
                "Overflow"
            }
            fromAccount.amount -= amount
            toAccount.amount += amount
        } finally {
            if (fromId < toId) {
                toAccount.lock.unlock()
                fromAccount.lock.unlock()
            } else {
                fromAccount.lock.unlock()
                toAccount.lock.unlock()
            }
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
        val lock = java.util.concurrent.locks.ReentrantLock()
    }
}