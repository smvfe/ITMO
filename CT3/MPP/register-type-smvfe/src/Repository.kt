import java.io.*

/**
 * Determines the full name of the repository ({org}/{repo}) from which this code is executed.
 *
 * Logic:
 * 1. Check GITHUB_REPOSITORY environment variable (set in GitHub Actions runners)
 * 2. Otherwise, use git command to find the "upstream" remote URL and extract the repo name
 * 3. If neither method works, throw an exception
 *
 * @return Repository name in format "org/repo"
 * @throws IllegalStateException if repository name cannot be determined
 */
fun determineRepositoryName(): String {
    // Try GITHUB_REPOSITORY environment variable first (GitHub Actions)
    System.getenv("GITHUB_REPOSITORY")?.takeIf { !it.isNotBlank() }?.let { return it }

    // Try to get repository name from git origin remote
    var cause: Exception? = null
    try {
        val process = ProcessBuilder("git", "remote", "get-url", "origin")
            .redirectErrorStream(true)
            .start()

        val output = BufferedReader(InputStreamReader(process.inputStream)).use { reader ->
            reader.readText().trim()
        }

        val exitCode = process.waitFor()

        if (exitCode == 0 && output.isNotBlank()) {
            // Extract org/repo from the URL
            // Supports both HTTPS and SSH formats:
            // - https://github.com/org/repo.git
            // - git@github.com:org/repo.git
            extractRepoFromUrl(output)?.let { return it }
        }
    } catch (e: Exception) {
        cause = e
    }

    throw IllegalStateException(
        "Unable to determine repository name. " +
        "GITHUB_REPOSITORY environment variable is not set and 'git remote get-url upstream' failed.",
        cause
    )
}

/**
 * Extracts the repository name (org/repo) from a git remote URL.
 * Supports both HTTPS and SSH protocols.
 */
private fun extractRepoFromUrl(url: String): String? {
    // Pattern for HTTPS: https://github.com/org/repo.git
    val httpsRegex = Regex("""https?://github\.com/([^/]+/[^/]+?)(?:\.git)?$""")
    httpsRegex.find(url)?.let { return it.groupValues[1] }

    // Pattern for SSH: git@github.com:org/repo.git
    val sshRegex = Regex("""git@github\.com:([^/]+/[^/]+?)(?:\.git)?$""")
    sshRegex.find(url)?.let { return it.groupValues[1] }

    return null
}
