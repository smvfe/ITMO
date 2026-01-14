import java.io.File

const val SOLUTION_FILE_NAME = "solution.txt"
const val REPO_NAME_TEMPLATE = "<org>/<repository-name>"
const val TYPE_PLACEHOLDER = "<type>"

fun readSolution(): CheckRequest {
    val lines = File(SOLUTION_FILE_NAME).readLines()

    // Remove comments and empty lines
    val cleanedLines = lines.mapIndexed { index, line ->
        IndexedValue(index + 1, line.substringBefore('#').trim())
    }.filter { it.value.isNotEmpty() }

    // First line is the repository name
    val name = cleanedLines.first().value

    // Parse histories
    val histories = mutableListOf<CheckHistory>()
    var i = 1

    while (i < cleanedLines.size) {
        // Read history type
        val (lineNo, typeLine) = cleanedLines[i]
        val type = if (typeLine == TYPE_PLACEHOLDER) {
            null
        } else {
            HistoryType.entries.firstOrNull { it.name.equals(typeLine, ignoreCase = true) } ?:
                error("$SOLUTION_FILE_NAME:$lineNo: Unknown type $typeLine")
        }
        i++

        // Read history lines (until we find a non-history line or end)
        val historyLines = mutableListOf<String>()
        while (i < cleanedLines.size && cleanedLines[i].value.matches(Regex("^[P-Z]: .*"))) {
            historyLines.add(cleanedLines[i].value)
            i++
        }
        histories.add(CheckHistory(type, historyLines))
    }

    return CheckRequest(name, histories)
}

fun writeSolution(solution: AssignResponse) {
    File(SOLUTION_FILE_NAME).printWriter().use { solutionOut ->
        solutionOut.apply {
            println("# This is your solution file.")
            println("# Analyze each history and replace <type> with the appropriate register type.")
            println("# Run `./gradlew build` to check your assignment.")
            println("# See README.md for details.")
            println()
            println("${solution.name} # Your repository (DO NOT EDIT)")

            solution.histories.forEachIndexed { index, history ->
                println()
                println("# History ${index + 1}")
                println("<type> # :TODO: REPLACE WITH ONE OF: linearizable, regular, safe, unsafe")
                history.lines.forEach { println(it) }
            }
        }
    }
}
