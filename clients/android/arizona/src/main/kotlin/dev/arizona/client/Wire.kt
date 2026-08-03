package dev.arizona.client

import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.int
import kotlinx.serialization.json.intOrNull
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive

/**
 * Pure wire-protocol logic, with no Android/Compose dependency, so it is
 * unit-testable on the JVM. Mirrors the browser worker's `zipTemplate`/
 * `resolveHtml` (assets/js/arizona-core.js) and the reference client
 * (e2e/utils/native_client.js): a fingerprint cache plus an interleaver that
 * stitches statics and dynamics into a JSON string.
 */

/** Diff op codes (mirror src/arizona.hrl). */
object Op {
    const val TEXT = 0
    const val SET_ATTR = 1
    const val REM_ATTR = 2
    const val REMOVE_NODE = 4
    const val INSERT = 5
    const val REMOVE = 6
    const val ITEM_PATCH = 7
    const val REPLACE = 8
    const val MOVE = 9
}

/** Effect command op codes (mirror include/arizona_effect.hrl). */
object Effect {
    const val PUSH_EVENT = 0
    const val NAVIGATE = 10
}

/**
 * Fingerprint-cache bound (mirrors `FP_CACHE_MAX` in assets/js/arizona-core.js).
 * A fingerprint hashes a template's statics, so every deploy mints a new key and
 * orphans the old one: unbounded, a long-lived client accumulates one generation
 * per deploy. Evicting is never wrong -- the cache is content-addressed, so the
 * server re-sends the statics for any key the client did not announce, and a miss
 * costs bytes only -- but it is only SAFE between connections: once a socket has
 * announced a key the server stops shipping that template's statics, so dropping
 * it mid-connection would leave a payload the client cannot resolve. The prune
 * therefore runs at announce time; the map is free to grow within a session.
 */
const val FP_CACHE_MAX = 1000

/**
 * Caches a fingerprint's statics so later frames can omit them (the server
 * sends `{f, d}` once `{f, s, d}` has been seen, deduplicated by fingerprint).
 */
class FingerprintCache {
    private data class Entry(val statics: List<String>, val t: Int?)

    // Access-ordered, so iteration is least-recently-used FIRST and the
    // announce-time prune just drops from the front.
    private val byFp = LinkedHashMap<String, Entry>(16, 0.75f, true)

    /** Statics for [payload], caching them if present, else from a prior frame. */
    fun statics(payload: JsonObject): List<String> {
        val f = payload.getValue("f").jsonPrimitive.content
        payload["s"]?.let { s ->
            byFp[f] = Entry(
                statics = s.jsonArray.map { it.jsonPrimitive.content },
                t = payload["t"]?.jsonPrimitive?.int,
            )
        }
        return byFp[f]?.statics ?: error("uncached fingerprint: $f")
    }

    /**
     * The cached fingerprints, most-recently-used LAST, after pruning down to
     * [limit]. Announced on every socket open so the server can omit statics this
     * client already holds.
     */
    fun announce(limit: Int = FP_CACHE_MAX): List<String> {
        val oldest = byFp.keys.iterator()
        while (byFp.size > limit) {
            oldest.next()
            oldest.remove()
        }
        return byFp.keys.toList()
    }
}

/**
 * Stitches a `{f, s, d}` payload into a JSON string, JSON-encoding each dynamic
 * value (string -> quoted, number -> as-is) -- the native counterpart of the
 * browser's string-concatenating zip. Recurses into nested `{f, s, d}` payloads
 * (child components) and each `{t: 0, ...}` item-lists.
 */
class Interleaver(private val cache: FingerprintCache) {

    fun interleave(payload: JsonObject): String = interleaveWith(cache.statics(payload), payload.getValue("d").jsonArray)

    /**
     * Decode an op payload to its JSON form: a `{t:0}` each-list -> a JSON array,
     * a `{f, s, d}` template -> a JSON object, a scalar -> itself. Used to
     * materialize OP_INSERT items and OP_TEXT content.
     */
    fun decode(payload: JsonElement): String = encodeValue(payload)

    private fun interleaveWith(statics: List<String>, dynamics: JsonArray): String {
        val sb = StringBuilder(statics[0])
        dynamics.forEachIndexed { i, v ->
            sb.append(encodeValue(v)).append(statics[i + 1])
        }
        return sb.toString()
    }

    private fun encodeValue(v: JsonElement): String =
        when {
            // An ?each expansion -> a JSON array of items the parent splices in.
            v is JsonObject && v["t"]?.jsonPrimitive?.intOrNull == 0 -> {
                val s = cache.statics(v)
                val items = v.getValue("d").jsonArray.map { interleaveWith(s, it.jsonArray) }
                items.joinToString(prefix = "[", postfix = "]", separator = ",")
            }
            // A nested {f, s, d} template (child component).
            v is JsonObject -> interleave(v)
            // A scalar: JsonPrimitive.toString() is its JSON form (quoted/bare).
            else -> (v as JsonPrimitive).toString()
        }
}
