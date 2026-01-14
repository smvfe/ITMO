import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.http.*
import kotlinx.coroutines.*
import kotlin.time.Duration.Companion.seconds

fun main(args: Array<String>) {
    val solution = readSolution()
    // Enable hint if passed as program argument
    if (args.contains("hint")) solution.hint = true

    if (solution.name == REPO_NAME_TEMPLATE) {
        error(
            "Problem is not assigned in $SOLUTION_FILE_NAME yet.\n" +
            "Run `./gradlew assign` to get your variant of the assignment."
        )
    }

    val firstMissing = solution.histories.indexOfFirst { it.type == null }
    if (firstMissing >= 0) {
        error(
            "History ${firstMissing + 1} is not solved in $SOLUTION_FILE_NAME yet.\n" +
            "Write its type in place of $TYPE_PLACEHOLDER first."
        )
    }

    val checkResponse = runBlocking {
        val httpResponse = createHttpClient().use { client ->
            client.post("$API_ADDRESS/check") {
                contentType(ContentType.Application.Json)
                setBody(solution)
            }
        }
        when {
            httpResponse.status == HttpStatusCode.TooManyRequests -> {
                val duration = httpResponse.headers[HttpHeaders.RetryAfter]?.let { value ->
                    // Assume Retry-After in seconds (delta-seconds)
                    value.toLongOrNull()?.seconds
                }
                val extra = duration?.let { d -> ", retry after $d" } ?: ""
                error("${httpResponse.status}$extra")
            }
            !httpResponse.status.isSuccess() -> {
                error(httpResponse.status.toString())
            }
            else -> httpResponse.body<CheckResponse>()
        }
    }

    if (!checkResponse.accepted) {
        error(checkResponse.message)
    }
}
