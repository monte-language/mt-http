import "lib/codec/utf8" =~ [=> UTF8 :DeepFrozen]
import "lib/enum" =~ [=> makeEnum :DeepFrozen]
import "lib/record" =~ [=> makeRecord :DeepFrozen]
import "unittest" =~ [=> unittest]
exports (Headers, makeHeaders, emptyHeaders, parseHeader)

# Common HTTP header structure.

# RFC 7230.

# 4. Transfer Codings
def [TransferCoding :DeepFrozen,
     IDENTITY :DeepFrozen,
     CHUNKED :DeepFrozen,
     COMPRESS :DeepFrozen,
     DEFLATE :DeepFrozen,
     GZIP :DeepFrozen,
] := makeEnum(["identity", "chunked", "compress", "deflate", "gzip"])

def parseTransferCoding(bs :Bytes) :List[TransferCoding] as DeepFrozen:
    return [for coding in (bs.toLowerCase().split(b`,`))
        switch (coding.trim()) {
            match b`identity` { IDENTITY }
            match b`chunked` { CHUNKED }
            match b`compress` { COMPRESS }
            match b`deflate` { DEFLATE }
            match b`gzip` { GZIP }
        }]

def testTransferCoding(assert):
    assert.equal(parseTransferCoding(b`identity`), [IDENTITY])
    assert.equal(parseTransferCoding(b`Chunked`), [CHUNKED])
    assert.equal(parseTransferCoding(b`gzip, chunked`), [GZIP, CHUNKED])

unittest([
    testTransferCoding,
])

def [Headers :DeepFrozen,
     makeHeaders :DeepFrozen] := makeRecord("Headers", [
    "contentLength" => NullOk[Int],
    "contentType" => NullOk[Pair[Str, Str]],
    "userAgent" => NullOk[Str],
    "transferCoding" => List[TransferCoding],
    "spareHeaders" => Map[Bytes, Bytes],
])

def emptyHeaders() :Headers as DeepFrozen:
    return makeHeaders(
        null, # contentLength
        null, # contentType
        null, # userAgent
        [], # transferCoding
        [].asMap())

def parseHeader(headers :Headers, bs :Bytes) :Headers as DeepFrozen:
    "Parse a bytestring header and add it to a header record."

    def b`@header:@{var value}` := bs
    value trim= ()
    return switch (header.trim().toLowerCase()):
        match b`content-length`:
            def len := _makeInt.fromBytes(value)
            headers.withContentLength(len)
        match b`content-type`:
            # XXX should support options, right?
            def via (UTF8.decode) `@type/@subtype` := value
            headers.withContentType([type, subtype])
        match b`transfer-coding`:
            headers.withTransferCoding(parseTransferCoding(value))
        match b`user-agent`:
            headers.withUserAgent(value)
        match h:
            def spareHeaders := headers.getSpareHeaders()
            headers.withSpareHeaders(spareHeaders.with(h, value))
