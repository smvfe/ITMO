import org.jetbrains.lincheck.datastructures.*
import org.junit.*

private const val N_ACCOUNTS = 5

/**
 * This test checks bank implementation for linearizability.
 */
class LinearizabilityTest() {
    @Param.Params(
        Param(name = "id", gen = IntGen::class, conf = "0:4"),
        Param(name = "amount", gen = LongGen::class, conf = "1:100")
    )
    open class BankOperations @JvmOverloads constructor(val bank: Bank = BankImpl(N_ACCOUNTS)) {
        @Operation(params = ["id"])
        fun amount(id: Int): Long =
            bank.amount(id)

        @get:Operation
        val totalAmount: Long
            get() = bank.totalAmount

        @Operation(params = ["id", "amount"])
        fun deposit(id: Int, amount: Long) =
            bank.deposit(id, amount)

        @Operation(params = ["id", "amount"])
        fun withdraw(id: Int, amount: Long) =
            bank.withdraw(id, amount)

        @Operation(params = ["id", "id", "amount"])
        fun transfer(idFrom: Int, idTo: Int, amount: Long) =
            bank.transfer(idFrom, idTo, amount)
    }

    class SequentialSpec : BankOperations(BankImplModel(N_ACCOUNTS))

    @Test
    fun testStress() {
        StressOptions()
            .sequentialSpecification(SequentialSpec::class.java)
            .check(BankOperations::class.java)
    }

    @Test
    fun testModel() {
        ModelCheckingOptions()
            .sequentialSpecification(SequentialSpec::class.java)
            .checkObstructionFreedom(true)
            .check(BankOperations::class.java)
    }
}
