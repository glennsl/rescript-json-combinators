type t<'a>

type fieldDecoders = {
  optional: 'a. (. string, t<'a>) => option<'a>,
  required: 'a. (. string, t<'a>) => 'a,
}

exception DecodeError(string)

module Error: {
  let expected: (string, Js.Json.t) => _
}

let custom: ((. Js.Json.t) => 'a) => t<'a>

let float: t<float>
let int: t<int>
let bool: t<bool>
let string: t<string>
let array: t<'a> => t<array<'a>>
let pair: (t<'a>, t<'b>) => t<('a, 'b)>
let option: t<'a> => t<option<'a>>
let field: (string, t<'a>) => t<'a>
let object: (fieldDecoders => 'a) => t<'a>

let oneOf: array<t<'a>> => t<'a>
let map: (t<'a>, (. 'a) => 'b) => t<'b>

let decode: (Js.Json.t, t<'a>) => result<'a, string>
