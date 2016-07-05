import "lib/codec/utf8" =~  [=> UTF8 :DeepFrozen]
import "lib/gai" =~ [=> makeGAI :DeepFrozen]
import "lib/enum" =~ [=> makeEnum :DeepFrozen]
import "lib/tubes" =~ [
    => makeMapPump :DeepFrozen,
    => makePumpTube :DeepFrozen,
]
import "http/headers" =~ [
    => Headers :DeepFrozen,
    => emptyHeaders :DeepFrozen,
    => parseHeader :DeepFrozen,
]
exports (main, makeRequest)

# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.


def lowercase(specimen, ej) as DeepFrozen:
    def s :Str exit ej := specimen
    return s.toLowerCase()

def finiteBody(headers :Headers) :Bool as DeepFrozen:
    return headers.getContentLength() != null

def smallBody(headers :Headers) :Bool as DeepFrozen:
    def contentLength := headers.getContentLength()
    return contentLength != null && contentLength < 1024 * 1024


def makeResponse(status :Int, headers :Headers, body) as DeepFrozen:
    return object response:
        to _printOn(out):
            out.print(`<response $status: $headers>`)

        to status() :Int:
            return status

        to headers() :Headers:
            return headers

        to body():
            return body


def [HTTPState :DeepFrozen,
     REQUEST :DeepFrozen,
     HEADER :DeepFrozen,
     BODY :DeepFrozen,
     BUFFERBODY :DeepFrozen,
     FOUNTBODY :DeepFrozen,
] := makeEnum(["request", "header", "body", "body (buffered)",
    "body (streaming)"])


def makeResponseDrain(resolver) as DeepFrozen:
    var state :HTTPState := REQUEST
    var buf :Bytes := b``
    var headers :Headers := emptyHeaders()
    var status :NullOk[Int] := null
    var label := null

    def bytesToInt(s, e):
        try:
            return _makeInt.fromBytes(s)
        catch p:
            e(p)

    return object responseDrain:
        to receive(bytes):
            buf += bytes
            responseDrain.parse()

        to flowingFrom(fount):
            return responseDrain

        to flowAborted(reason):
            traceln(`Flow aborted: $reason`)

        to flowStopped(reason):
            traceln(`End of response: $reason`)

        to parseStatus(ej):
            def b`HTTP/1.1 @{via (bytesToInt) statusCode} @{via (UTF8.decode) label}$\r$\n@tail` exit ej := buf
            status := statusCode
            traceln(`Status: $status ($label)`)
            buf := tail
            state := HEADER
            headers := emptyHeaders()

        to parseHeader(ej):
            def index := buf.indexOf(b`$\r$\n`)

            if (index == -1):
                throw.eject(ej, "No newline")

            if (index == 0):
                # Single newline; end of headers.
                buf := buf.slice(2)
                state := BODY

            def slice := buf.slice(0, index)
            headers := parseHeader(headers, slice)
            buf := buf.slice(index + 2)

        to parse():
            while (true):
                switch (state):
                    match ==REQUEST:
                        responseDrain.parseStatus(__break)
                    match ==HEADER:
                        responseDrain.parseHeader(__break)
                    match ==BODY:
                        if (finiteBody(headers)):
                            traceln("Currently expecting finite body")
                            if (smallBody(headers)):
                                traceln("Body is small; will buffer in memory")
                                state := BUFFERBODY
                            else:
                                traceln("Body isn't small")
                                state := FOUNTBODY
                    match ==BUFFERBODY:
                        def contentLength := headers.getContentLength()
                        if (buf.size() >= contentLength):
                            def body := buf.slice(0, contentLength)
                            buf := buf.slice(contentLength, buf.size())
                            responseDrain.finalize(body)
                        else:
                            break
                    match ==FOUNTBODY:
                        traceln("I'm not prepared to do this yet!")
                        throw("Couldn't do fount body!")

        to finalize(body):
            def response := makeResponse(status, headers, body)
            resolver.resolve(response)


def makeRequest(makeTCP4ClientEndpoint, host :Bytes, resource :Str,
                => port :Int := 80) as DeepFrozen:
    def headers := [
        "Host" => host,
        "Connection" => b`close`,
    ].diverge()

    return object request:
        to put(key, value :Bytes):
            headers[key] := value

        to write(verb, drain):
            drain.receive(UTF8.encode(`$verb $resource HTTP/1.1$\r$\n`, null))
            for via (UTF8.encode) k => v in (headers):
                drain.receive(b`$k: $v$\r$\n`)
            drain.receive(b`$\r$\n`)

        to send(verb :Str):
            def endpoint := makeTCP4ClientEndpoint(host, port)
            def [fount, drain] := endpoint.connect()
            def [p, r] := Ref.promise()

            # Write request.
            when (drain) ->
                request.write(verb, drain)
            # Read response.
            fount<-flowTo(makeResponseDrain(r))

            return p

        to get():
            return request.send("GET")


def main(argv, => getAddrInfo, => makeTCP4ClientEndpoint) as DeepFrozen:
    def addrs := getAddrInfo(b`example.com`, b``)
    return when (addrs) ->
        def gai := makeGAI(addrs)
        def [addr] + _ := gai.TCP4()
        def response := makeRequest(makeTCP4ClientEndpoint, addr.getAddress(), "/").get()
        when (response) ->
            traceln("Finished request with response", response)
            traceln(UTF8.decode(response.body(), null))
            0
