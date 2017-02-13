import "lib/codec/utf8" =~ [=> UTF8 :DeepFrozen]
import "lib/enum" =~ [=> makeEnum :DeepFrozen]
import "lib/record" =~ [=> makeRecord :DeepFrozen]
import "unittest" =~ [=> unittest]
exports (Headers, makeHeaders, emptyHeaders, parseHeader,
         IDENTITY, CHUNKED)

# Common HTTP header structure.

# RFC 7230.

# 4. Transfer Encodings
def [TransferEncoding :DeepFrozen,
     IDENTITY :DeepFrozen,
     CHUNKED :DeepFrozen,
     COMPRESS :DeepFrozen,
     DEFLATE :DeepFrozen,
     GZIP :DeepFrozen,
] := makeEnum(["identity", "chunked", "compress", "deflate", "gzip"])

def parseTransferEncoding(bs :Bytes) :List[TransferEncoding] as DeepFrozen:
    return [for coding in (bs.toLowerCase().split(b`,`))
        switch (coding.trim()) {
            match b`identity` { IDENTITY }
            match b`chunked` { CHUNKED }
            match b`compress` { COMPRESS }
            match b`deflate` { DEFLATE }
            match b`gzip` { GZIP }
        }]

def testTransferEncoding(assert):
    assert.equal(parseTransferEncoding(b`identity`), [IDENTITY])
    assert.equal(parseTransferEncoding(b`Chunked`), [CHUNKED])
    assert.equal(parseTransferEncoding(b`gzip, chunked`), [GZIP, CHUNKED])

unittest([
    testTransferEncoding,
])

def [Headers :DeepFrozen,
     makeHeaders :DeepFrozen] := makeRecord("Headers", [
    "contentLength" => NullOk[Int],
    "contentType" => NullOk[Pair[Str, Str]],
    "userAgent" => NullOk[Str],
    "transferEncoding" => List[TransferEncoding],
    "spareHeaders" => Map[Bytes, Bytes],
])

def emptyHeaders() :Headers as DeepFrozen:
    return makeHeaders(
        null, # contentLength
        null, # contentType
        null, # userAgent
        [], # transferEncoding
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
        match b`transfer-encoding`:
            headers.withTransferEncoding(parseTransferEncoding(value))
        match b`user-agent`:
            headers.withUserAgent(value)
        match h:
            def spareHeaders := headers.getSpareHeaders()
            headers.withSpareHeaders(spareHeaders.with(h, value))

def testParseHeaderTransferEncoding(assert):
    def headers := parseHeader(emptyHeaders(), b`Transfer-Encoding: chunked`)
    assert.equal(headers.getTransferEncoding(), [CHUNKED])

unittest([
    testParseHeaderTransferEncoding,
])
