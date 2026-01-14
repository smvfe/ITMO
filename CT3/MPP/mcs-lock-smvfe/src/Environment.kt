import java.util.concurrent.atomic.AtomicReference

/**
 * Окружение в котором происходит тестирование.
 */
interface Environment {
    fun park()
    fun unpark(thread: Thread)
}

/**
 * Свойство для более удобной работы с [AtomicReference] из Kotlin.
 */
var <T> AtomicReference<T>.value: T
    get() = get()
    set(value) = set(value)
