/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :TODO: Мочеков Семён
 */
class Solution : MonotonicClock {
    private var c11 by RegularInt(0) // d1 (L)
    private var c21 by RegularInt(0) // d2 (L)
    private var c31 by RegularInt(0) // d3 (L)
    private var c12 by RegularInt(0) // d1 (R)
    private var c22 by RegularInt(0) // d2 (R)
    private var c32 by RegularInt(0) // d3 (R)

    override fun write(time: Time) {
        c11 = time.d1
        c21 = time.d2
        c31 = time.d3
        c32 = time.d3
        c22 = time.d2
        c12 = time.d1
    }

    override fun read(): Time {
        val d1 = c12
        val d2 = c22
        val d3 = c32

        val r1 = c11
        if (r1 != d1) return Time(r1, 0, 0)

        val r2 = c21
        if (r2 != d2) return Time(d1, r2, 0)

    val r3 = c31
    if (r3 != d3) return Time(d1, d2, maxOf(r3, d3))

        return Time(d1, d2, d3)
    }
}
