type t<'a> = 'a => Js.Json.t

external int: int => Js.Json.t = "%identity"
external float: float => Js.Json.t = "%identity"
external bool: bool => Js.Json.t = "%identity"
external string: string => Js.Json.t = "%identity"

external jsonDict: Js.Dict.t<Js.Json.t> => Js.Json.t = "%identity"
external jsonArray: array<Js.Json.t> => Js.Json.t = "%identity"
external stringArray: array<string> => Js.Json.t = "%identity"
external intArray: array<int> => Js.Json.t = "%identity"
external floatArray: array<float> => Js.Json.t = "%identity"
external boolArray: array<bool> => Js.Json.t = "%identity"

@val external null: Js.Json.t = "null"

let array = encode => arr => arr->Js.Array2.map(x => encode(x))->jsonArray

let list = encode => l =>
  switch l {
  | list{} => jsonArray([])
  | list{hd, ...tl} =>
    let arr = Array.make(l->List.length, hd->encode)
    let rec fill = (i, l) =>
      switch l {
      | list{} => arr
      | list{hd, ...tl} =>
        Array.unsafe_set(arr, i, hd->encode)
        fill(i + 1, tl)
      }
    fill(1, tl)->jsonArray
  }

let object = props => props->Js.Dict.fromArray->jsonDict

let option = encode => opt =>
  switch opt {
  | None => null
  | Some(v) => v->encode
  }

let withDefault = (default, encode) => opt =>
  switch opt {
  | None => default
  | Some(v) => v->encode
  }

let date = date => date->Js.Date.toJSONUnsafe->string

let pair = (encodeA, encodeB) => ((a, b)) => [a->encodeA, b->encodeB]->jsonArray
let tuple2 = (encodeA, encodeB) => ((a, b)) => [a->encodeA, b->encodeB]->jsonArray
let tuple3 = (encodeA, encodeB, encodeC) => ((a, b, c)) =>
  [a->encodeA, b->encodeB, c->encodeC]->jsonArray
let tuple4 = (encodeA, encodeB, encodeC, encodeD) => ((a, b, c, d)) =>
  [a->encodeA, b->encodeB, c->encodeC, d->encodeD]->jsonArray

let dict = encode => dict => Js.Dict.map(v => encode(v), dict)->jsonDict

module Unsafe = {
  external object: {..} => Js.Json.t = "%identity"
}
