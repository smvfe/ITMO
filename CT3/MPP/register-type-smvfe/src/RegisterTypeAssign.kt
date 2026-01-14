import io.ktor.client.call.*
import io.ktor.client.request.*
import kotlinx.coroutines.*

fun main() {
    val solution = readSolution()
    if (solution.name != REPO_NAME_TEMPLATE) {
        error(
            "Problem is already assigned for ${solution.name}.\n" +
            "Solve and verify it."
        )
    }
    val repository = determineRepositoryName()
    val response = runBlocking {
        createHttpClient().use { client ->
            client.get("$API_ADDRESS/assign?name=$repository").body<AssignResponse>()
        }
    }
    writeSolution(response)
    println("Assignment retrieved for ${response.name}")
    println("Edit $SOLUTION_FILE_NAME to provide your solution")
}
