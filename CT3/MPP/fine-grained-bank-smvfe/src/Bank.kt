/**
 * Bank interface.
 */
interface Bank {
    /**
     * Returns the number of accounts in this bank.
     */
    val accountsCount: Int

    /**
     * Returns current amount in the specified account.
     *
     * @param id account index from 0 to [n][accountsCount]-1.
     * @return amount in account.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     */
    fun amount(id: Int): Long

    /**
     * Returns total amount deposited in this bank.
     */
    val totalAmount: Long

    /**
     * Deposits specified amount to account.
     *
     * @param id account index from 0 to [n][accountsCount]-1.
     * @param amount positive amount to deposit.
     * @return resulting amount in account.
     * @throws IllegalArgumentException when amount <= 0.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     * @throws IllegalStateException when deposit will overflow account above [MAX_AMOUNT].
     */
    fun deposit(id: Int, amount: Long): Long

    /**
     * Withdraws specified amount from account.
     *
     * @param id account index from 0 to [n][accountsCount]-1.
     * @param amount positive amount to withdraw.
     * @return resulting amount in account.
     * @throws IllegalArgumentException when amount <= 0.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     * @throws IllegalStateException when account does not enough to withdraw.
     */
    fun withdraw(id: Int, amount: Long): Long

    /**
     * Transfers specified amount from one account to another account.
     *
     * @param fromId account index to withdraw from.
     * @param toId account index to deposit to.
     * @param amount positive amount to transfer.
     * @throws IllegalArgumentException when amount <= 0 or fromIndex == toIndex.
     * @throws IndexOutOfBoundsException when account indices are invalid.
     * @throws IllegalStateException when there is not enough funds in source account or too much in target one.
     */
    fun transfer(fromId: Int, toId: Int, amount: Long)

    companion object {
        /**
         * The maximal amount that can be kept in a bank account.
         */
        const val MAX_AMOUNT = 1000000000000000L
    }
}