type t<'a> = 'a => Js.Json.t

external int: int => Js.Json.t = "%identity"
external float: float => Js.Json.t = "%identity"
external bool: bool => Js.Json.t = "%identity"
external string: string => Js.Json.t = "%identity"

@val external null: Js.Json.t = "null"
let array: t<'a> => t<array<'a>>
let list: t<'a> => t<list<'a>>
let object: array<(string, Js.Json.t)> => Js.Json.t
let option: t<'a> => t<option<'a>>
let withDefault: (Js.Json.t, t<'a>, option<'a>) => Js.Json.t

let date: Js.Date.t => Js.Json.t
let pair: (t<'a>, t<'b>, ('a, 'b)) => Js.Json.t
let tuple2: (t<'a>, t<'b>, ('a, 'b)) => Js.Json.t
let tuple3: (t<'a>, t<'b>, t<'c>, ('a, 'b, 'c)) => Js.Json.t
let tuple4: (t<'a>, t<'b>, t<'c>, t<'d>, ('a, 'b, 'c, 'd)) => Js.Json.t
let dict: (t<'a>, Js.Dict.t<'a>) => Js.Json.t

// Short-hands for zero-cost composite primitives
external jsonDict: Js.Dict.t<Js.Json.t> => Js.Json.t = "%identity"
external jsonArray: array<Js.Json.t> => Js.Json.t = "%identity"
external stringArray: array<string> => Js.Json.t = "%identity"
external intArray: array<int> => Js.Json.t = "%identity"
external floatArray: array<float> => Js.Json.t = "%identity"
external boolArray: array<bool> => Js.Json.t = "%identity"

module Unsafe: {
  external object: {..} => Js.Json.t = "%identity"
}
