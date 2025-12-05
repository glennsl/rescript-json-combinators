type t<'a> = 'a => JSON.t

external int: int => JSON.t = "%identity"
external float: float => JSON.t = "%identity"
external bool: bool => JSON.t = "%identity"
external string: string => JSON.t = "%identity"

external jsonDict: dict<JSON.t> => JSON.t = "%identity"
external jsonArray: array<JSON.t> => JSON.t = "%identity"
external stringArray: array<string> => JSON.t = "%identity"
external intArray: array<int> => JSON.t = "%identity"
external floatArray: array<float> => JSON.t = "%identity"
external boolArray: array<bool> => JSON.t = "%identity"

@val external null: JSON.t = "null"

let array = encode => arr => arr->Array.map(x => encode(x))->jsonArray

let list = encode =>
  l =>
    switch l {
    | list{} => jsonArray([])
    | list{hd, ...tl} =>
      let arr = Array.make(~length=l->List.length, hd->encode)
      let rec fill = (i, l) =>
        switch l {
        | list{} => arr
        | list{hd, ...tl} =>
          Array.setUnsafe(arr, i, hd->encode)
          fill(i + 1, tl)
        }
      fill(1, tl)->jsonArray
    }

let object = props => props->Dict.fromArray->jsonDict

let option = encode =>
  opt =>
    switch opt {
    | None => null
    | Some(v) => v->encode
    }

let withDefault = (default, encode) =>
  opt =>
    switch opt {
    | None => default
    | Some(v) => v->encode
    }

let date = date => date->Date.toJSON->Option.getUnsafe->string

let pair = (encodeA, encodeB) => ((a, b)) => [a->encodeA, b->encodeB]->jsonArray
let tuple2 = (encodeA, encodeB) => ((a, b)) => [a->encodeA, b->encodeB]->jsonArray
let tuple3 = (encodeA, encodeB, encodeC) =>
  ((a, b, c)) => [a->encodeA, b->encodeB, c->encodeC]->jsonArray
let tuple4 = (encodeA, encodeB, encodeC, encodeD) =>
  ((a, b, c, d)) => [a->encodeA, b->encodeB, c->encodeC, d->encodeD]->jsonArray

let dict = encode => dict => dict->Dict.mapValues(encode)->jsonDict

module Unsafe = {
  external object: {..} => JSON.t = "%identity"
}
