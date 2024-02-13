# rescript-json-combinators
Combinator library for JSON decoding and encoding.

[![npm](https://img.shields.io/npm/v/@glennsl/rescript-json-combinators.svg)](https://npmjs.org/@glennsl/rescript-json-combinators)
[![Issues](https://img.shields.io/github/issues/glennsl/rescript-json-combinators.svg)](https://github.com/glennsl/rescript-json-combinators/issues)
[![Last Commit](https://img.shields.io/github/last-commit/glennsl/rescript-json-combinators.svg)](https://github.com/glennsl/rescript-json-combinators/commits/master)


## Example

```rescript
type point = {
  x: int,
  y: int,
}

type polyline = {
  points: array<point>,
  thickness: option<int>,
}

module Decode = {
  open Json.Decode

  let point = object(field => {
    x: field.required(. "x", int),
    y: field.required(. "y", int),
  })

  let polyline = object(field => {
    points: field.required(. "points", array(point)),
    thickness: field.optional(. "thickness", int),
  })
}

let data = `{
  "points": [
    { "x": 1, "y": -4 },
    { "x": 5, "y": 8 }
  ]
}`

let _ = data->Js.Json.parseExn->Json.decode(Decode.polyline)->Js.log
```

See [examples](https://github.com/glennsl/rescript-json-combinators/blob/master/examples/) for more.


## Installation

```sh
npm install --save @glennsl/rescript-json-combinators
```

Then add `@glennsl/rescript-json-combinators` to `bs-dependencies` in your `bsconfig.json`:

```diff
 {
   "bs-dependencies": [
+    "@glennsl/rescript-json-combinators"
   ]
 }
```


Optionally, add this to automatically open the library in order to use `Json.Encode` and `Json.Decode` directly:

```diff
 {
   "bsc-flags": [
+    "-open JsonCombinators",
   ]
 }
```


## Differences from bs-json

`rescript-json-combinators` is the spriritual successor of `bs-json`. It was rewritten from scratch and not intended to be 
backwards compatible. And while for the most part is does share the same design principles,there are also some notable differences:

* Decoders have been made abstract. While the internal representation is still the same, a function that takes a `Js.Json.t`
and returns either the decoded value or raises an exception, they can no longer be called directly. Instead they have to be run
via `Decode.decode`. This is in order to ensure that the `DecodeError` exceptions don't leak and returns a `result`
instead. Custom decoders can still be created with `Decode.custom`.

* A new `object` decoder has been added, and while it's still possible to decode fields individually in a custom decoder, this
addresses a couple ergonomic problems with that approach, such as distinguishing between a field missing and having the wrong
type, and the over-use and under-encapsulation of custom decoders.

* Uncurrying and raw javascript is used in some places to improve performance. For the most part this isn't visible to the
end user because decoders can no longer be called directly. There are a couple exceptions however, such as field decoders inside
the `object` deocder, and `map`.


## Documentation

### API

For the moment, please see the interface files:

* [Json](https://github.com/glennsl/rescript-json-combinators/blob/master/src/Json.resi)
* [Json.Encode](https://github.com/glennsl/rescript-json-combinators/blob/master/src/Json_Encode.resi)
* [Json.Decode](https://github.com/glennsl/rescript-json-combinators/blob/master/src/Json_Decode.resi)


## License

This work is dual-licensed under [LGPL 3.0](https://choosealicense.com/licenses/lgpl-3.0/) and 
[MPL 2.0](https://choosealicense.com/licenses/mpl-2.0/). You can choose between one of them if you use this work.

Please see [LICENSE.LGPL-3.0](https://github.com/glennsl/rescript-json-combinators/blob/master/LICENSE.LGPL-3.0) and 
[LICENSE.MPL-2.0](https://github.com/glennsl/rescript-json-combinators/blob/master/LICENSE.MPL-2.0) for the full text of each license.

`SPDX-License-Identifier: LGPL-3.0 OR MPL-2.0`


## Changes

### 1.4.0
- explicitly curried encode combinators to reduce breakage in rescript 11 uncurried mode
- updated examples to rescript 11

### 1.3.0
- add `Decode.indirect` to help with decoding recursive data structures

### 1.2.0
- add `Decode.flatMap`

### 1.1.0
- recript v11-compatible formatting (#3)

### 1.0.0
Initial release
