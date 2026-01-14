import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.serialization.kotlinx.json.*

const val API_ADDRESS = "https://coursemgr.vsegda.org/mpp/register-type/v1"

fun createHttpClient(): HttpClient = HttpClient(CIO) {
    install(ContentNegotiation) {
        json()
    }
}
