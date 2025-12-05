type t<'a> = 'a => JSON.t

external int: int => JSON.t = "%identity"
external float: float => JSON.t = "%identity"
external bool: bool => JSON.t = "%identity"
external string: string => JSON.t = "%identity"

@val external null: JSON.t = "null"
let array: t<'a> => t<array<'a>>
let list: t<'a> => t<list<'a>>
let object: array<(string, JSON.t)> => JSON.t
let option: t<'a> => t<option<'a>>
let withDefault: (JSON.t, t<'a>) => option<'a> => JSON.t

let date: Date.t => JSON.t
let pair: (t<'a>, t<'b>) => (('a, 'b)) => JSON.t
let tuple2: (t<'a>, t<'b>) => (('a, 'b)) => JSON.t
let tuple3: (t<'a>, t<'b>, t<'c>) => (('a, 'b, 'c)) => JSON.t
let tuple4: (t<'a>, t<'b>, t<'c>, t<'d>) => (('a, 'b, 'c, 'd)) => JSON.t
let dict: t<'a> => Js.dict<'a> => JSON.t

// Short-hands for zero-cost composite primitives
external jsonDict: Js.dict<JSON.t> => JSON.t = "%identity"
external jsonArray: array<JSON.t> => JSON.t = "%identity"
external stringArray: array<string> => JSON.t = "%identity"
external intArray: array<int> => JSON.t = "%identity"
external floatArray: array<float> => JSON.t = "%identity"
external boolArray: array<bool> => JSON.t = "%identity"

module Unsafe: {
  external object: {..} => JSON.t = "%identity"
}
