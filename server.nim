import
    std/[
        asynchttpserver, asyncdispatch, tables, strtabs, parseutils, strutils, base64,
        osproc, streams, tempfiles,
    ]
import pkg/[QRgen, pixie, QRgen/renderer]

let server = newAsyncHttpServer()

when defined(release):
    let port = Port(80)
else:
    let port = Port(2005)

server.listen(port)

echo "Server started on port ", $port.uint16

type MultiData = OrderedTable[string, seq[tuple[fields: StringTableRef, body: string]]]

template parseContentDisposition() {.dirty.} =
    var hCount = 0
    while hCount < hValue.len() - 1:
        var key = ""
        hCount += hValue.parseUntil(key, {';', '='}, hCount)
        if hValue[hCount] == '=':
            var value = hvalue.captureBetween('"', start = hCount)
            hCount += value.len + 2
            inc(hCount) # Skip ;
            hCount += hValue.skipWhitespace(hCount)
            if key == "name":
                name = value
            newPart[0][key] = value
        else:
            inc(hCount)
            hCount += hValue.skipWhitespace(hCount)

proc parseMultiPart(body: string, boundary: string): MultiData =
    result =
        initOrderedTable[string, seq[tuple[fields: StringTableRef, body: string]]]()
    var mboundary = "--" & boundary

    var i = 0
    var partsLeft = true
    while partsLeft:
        var firstBoundary = body.skip(mboundary, i)
        if firstBoundary == 0:
            raise newException(
                ValueError, "Expected boundary. Got: " & body.substr(i, i + 25)
            )
        i += firstBoundary
        i += body.skipWhitespace(i)

        # Headers
        var newPart: tuple[fields: StringTableRef, body: string] =
            ({:}.newStringTable, "")
        var name = ""
        while true:
            if body[i] == '\c':
                inc(i, 2) # Skip \c\L
                break
            var hName = ""
            i += body.parseUntil(hName, ':', i)
            if body[i] != ':':
                raise newException(ValueError, "Expected : in headers.")
            inc(i) # Skip :
            i += body.skipWhitespace(i)
            var hValue = ""
            i += body.parseUntil(hValue, {'\c', '\L'}, i)
            if toLowerAscii(hName) == "content-disposition":
                parseContentDisposition()
            newPart[0][hName] = hValue
            i += body.skip("\c\L", i) # Skip *one* \c\L

        # Parse body.
        while true:
            if body[i] == '\c' and body[i + 1] == '\L' and
                    body.skip(mboundary, i + 2) != 0:
                if body.skip("--", i + 2 + mboundary.len) != 0:
                    partsLeft = false
                    break
                break
            else:
                newPart[1].add(body[i])
            inc(i)
        i += body.skipWhitespace(i)

        discard result.hasKeyOrPut(name, @[])
        result[name].add(newPart)

proc parseMPFD(contentType: string, body: string): MultiData =
    var boundaryEqIndex = contentType.find("boundary=") + 9
    var boundary = contentType.substr(boundaryEqIndex, contentType.len() - 1)
    return parseMultiPart(body, boundary)

proc formData(req: Request): MultiData =
    let contentType = req.headers.getOrDefault("Content-Type")
    if contentType.startsWith("multipart/form-data"):
        result = parseMPFD(contentType, req.body)

proc requestCallBack(req: Request) {.async, gcsafe.} =
    let
        txtHeaders = {"Content-type": "text/plain"}
        pngHeaders = {"Content-type": "image/png"}
        jsHeaders = {"Content-type": "text/javascript"}
        svgHeaders = {"Content-type": "image/svg+xml"}

    case req.url.path
    of "/":
        let
            indexPage = readfile("index.html")
            htmlHeaders = {"Content-type": "text/html; charset=utf-8"}

        await req.respond(Http200, indexPage, htmlHeaders.newHttpHeaders())
    of "/styles.css":
        let
            styleCss = readfile("styles.css")
            cssHeaders = {"Content-type": "text/css"}

        await req.respond(Http200, styleCss, cssHeaders.newHttpHeaders())
    of "/qrcode":
        try:
            if req.reqMethod == HttpPost:
                let
                    formData = req.formData()
                    brandLogoData = formData["brandLogo"][0]
                    brandLogoDataType = brandLogoData.fields["Content-Type"]
                    brandDominantColor = formData["dominantColor"][0].body
                    brandUrl = formData["brandUrl"][0].body
                    qrCode = newQR(brandUrl, ecLevel = qrECH)
                case brandLogoDataType
                of "image/png", "image/jpeg":
                    let
                        imgEmbed = decodeImage(brandLogoData.body)
                        png = qrCode
                            .renderImg(
                                dark = brandDominantColor,
                                alRad = 20,
                                moRad = 10,
                                img = imgEmbed,
                            )
                            .encodeImage(PngFormat)

                    await req.respond(
                        Http200, png.encodeMime, txtHeaders.newHttpHeaders()
                    )
                of "image/svg+xml":
                    let tempFile = createTempFile("myTemp_", ".png", dir = "temp")
                    discard execProcess(
                        "echo " &
                            qrCode.printSvg(
                                dark = brandDominantColor,
                                alRad = 20,
                                moRad = 10,
                                svgImg = brandLogoData.body,
                            ).quoteShell & " | " & "rsvg-convert --width=300 -o " &
                            tempFile.path
                    )

                    let png = readFile(tempFile.path)

                    await req.respond(
                        Http200, png.encodeMime, txtHeaders.newHttpHeaders()
                    )
                    close(tempFile.cfile)
            else:
                await req.respond(
                    Http405,
                    "Error 405: This Route Accepts Only POST requests with formdata",
                )
        except:
            await req.respond(
                Http500,
                "An Error Occured, Please Use Another File or Change The File Format",
                txtHeaders.newHttpHeaders(),
            )
    else:
        await req.respond(Http404, "Error 404: This Route Does Not Exist")

while true:
    if server.shouldAcceptRequest:
        waitFor server.acceptRequest(requestCallBack)
    else:
        waitFor sleepAsync(500)
