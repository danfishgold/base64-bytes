# Base64

This is an Elm package for encoding and decoding Base64 strings.

You can [read more about Base64 on Wikipedia][wiki],
but basically you can use Base64 to encode bytes in plain text.

This package allows you to convert bytes to Base64 strings with `Base64.fromBytes`
and convert a Base64 string to bytes with `Base64.toBytes`.

[View demo](https://danfishgold.github.io/base64-bytes)

## Usage

    elm install danfishgold/base64-bytes

### Bytes to Base64

```elm

import Bytes.Encode
import Base64

Bytes.Encode.string "Hello World"
    |> Bytes.Encode.encode
    |> Base64.fromBytes

--> Just "SGVsbG8gV29ybGQ="

Bytes.Encode.string "אהלן עולם"
    |> Bytes.Encode.encode
    |> Base64.fromBytes

--> Just "15DXlNec158g16LXldec150="

```

### Base64 to Bytes

```elm

import Bytes
import Bytes.Decode
import Base64

case Base64.toBytes "SGVsbG8gV29ybGQ=" of
    Nothing ->
        Just "Invalid Base64 string"
    Just bytes ->
        Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes

--> Just "Hello World"


```

## Example

I've written a more complete example where the user can enter text on a web page
and the url updates accordingly, so you can refresh the page and maintain the state.

You can [see the demo][demo] or [view the source][source]

[wiki]: https://en.wikipedia.org/wiki/Base64
[demo]: https://danfishgold.github.io/base64-bytes
[source]: https://github.com/danfishgold/base64-bytes
