import kotlinx.serialization.Serializable

enum class HistoryType {
    UNSAFE,
    SAFE,
    REGULAR,
    LINEARIZABLE
}

@Serializable
data class AssignResponse(
    val name: String,
    val histories: List<AssignHistory>
)

@Serializable
data class AssignHistory(
    val lines: List<String>
)

@Serializable
data class CheckRequest(
    val name: String,
    val histories: List<CheckHistory>,
    var hint: Boolean = false
)

@Serializable
data class CheckHistory(
    val type: HistoryType?,
    val lines: List<String>
)

@Serializable
data class CheckResponse(
    val name: String,
    val accepted: Boolean,
    val message: String
)
