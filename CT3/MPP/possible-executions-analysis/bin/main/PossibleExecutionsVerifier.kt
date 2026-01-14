import java.io.*
import java.util.*

const val SOLUTION_FILE_NAME = "solution.txt"
val STATE_REGEX = Regex("\\[P([1-3]),Q([1-3]),([01])]")

fun main() {
    val transitions = mutableSetOf<Transition>()
    File(SOLUTION_FILE_NAME).readLines().forEachIndexed { index, line ->
        val trim = line.substringBefore('#').trim()
        if (index == 0) {
            require(trim != "<Last-Name> <First-Name>") { "The first line must be filled in with name" }
            println("### Validating $SOLUTION_FILE_NAME ### $trim")
        } else {
            try {
                if (trim.isNotEmpty()) {
                    val t = parseTransition(trim)
                    require(transitions.add(t)) { "Duplicate transition $t" }
                }
            } catch (e: IllegalArgumentException) {
                error("At $SOLUTION_FILE_NAME:${index + 1}: ${e.message}")
            }
        }
    }
    val states = transitions
        .groupBy({ it.from }, { it.to })
        .mapValues { it.value.toSet() }
        .toMutableMap()
    for (s in states.values.flatten()) {
        if (s !in states) states[s] = emptySet()
    }
    val initial = State(1, 1, 0)
    // check initial state
    require(initial in states) { "Must contain transition from initial state $initial" }
    // check complete transitions out of each state
    for ((from, tos) in states) {
        val expected = mutableSetOf<State>()
        from.moveP().let { expected += it }
        from.moveQ()?.let { expected += it }
        require(expected.size == tos.size) { "Unexpected number of transitions (${tos.size}) from state $from" }
        for (e in expected) {
            require(e in tos) { "Missing transition from state $from" }
        }
    }
    // check reachability of all states
    val queue = ArrayDeque<State>()
    val reached = HashSet<State>()
    fun mark(state: State) { if (reached.add(state)) queue += state }
    mark(initial)
    while (!queue.isEmpty()) {
        val from = queue.removeFirst()
        for (to in states[from]!!) mark(to)
    }
    for (state in states.keys) {
        require(state in reached) { "State $state in never reached from the initial state" }
    }
}

data class State(val p: Int, val q: Int, val a: Int) {
    override fun toString(): String = "[P$p,Q$q,$a]"

    fun moveP(): State = when(p) {                    // while true:
        1 -> copy(p = 2, a = 0)                       //   1: a = 0
        2 -> if (a == 0) this else copy(p = 3)        //   2: while a == 0: pass
        3 -> copy(p = 1)                              //   3: pass // critical section, do nothing
        else -> error("Invalid state $this")
    }

    fun moveQ(): State? = when(q) {                   // while true:
        1 -> copy(q = 2, a = 1)                       //   1: a = 1
        2 -> if (a == 1) this else copy(q = 3)        //   2: while a == 1: pass
        3 -> copy(q = 1)                              //   3: pass // critical section, do nothing
        else -> error("Invalid state $this")
    }
}

data class Transition(val from: State, val to: State) {
    override fun toString(): String = "$from -> $to"
}

fun parseTransition(s: String): Transition {
    val i = s.indexOf("->")
    require(i > 0) { "Must contain transition with '->' separator" }
    return Transition(parseState(s.substring(0, i)), parseState(s.substring(i + 2)))
}

fun parseState(s: String): State {
    val match = STATE_REGEX.matchEntire(s.trim())
    require(match != null) { "State does not match a specified format $STATE_REGEX" }
    val g = match.groupValues.drop(1).map { it.toInt() }
    return State(g[0], g[1], g[2])
}

