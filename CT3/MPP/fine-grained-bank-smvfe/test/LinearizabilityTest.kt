import org.jetbrains.lincheck.datastructures.*
import org.junit.*

/**
 * This test checks bank implementation for linearizability.
 */
@Param(name = "id", gen = IntGen::class, conf = "0:4")
@Param(name = "amount", gen = LongGen::class, conf = "1:100")
class LinearizabilityTest {
    private val bank: Bank = BankImpl(5)

    init {
        configureLock(bank)
    }

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
    fun transfer(fromId: Int, toId: Int, amount: Long) =
         bank.transfer(fromId, toId, amount)

    @Test
    fun stressTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(10_000)
        .threads(3)
        .actorsPerThread(2)
        .actorsBefore(2)
        .actorsAfter(2)
        .check(this::class.java)
}