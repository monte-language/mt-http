import "lib/record" =~ [=> makeRecord :DeepFrozen]
exports (Headers, makeHeaders, emptyHeaders)

# Common HTTP header structure.

def [Headers :DeepFrozen,
     makeHeaders :DeepFrozen] := makeRecord("Headers", [
    "contentLength" => NullOk[Int],
    "contentType" => NullOk[Pair[Str, Str]],
    "spareHeaders" => Map[Str, Str],
])

def emptyHeaders() :Headers as DeepFrozen:
    return makeHeaders(null, null, [].asMap())
