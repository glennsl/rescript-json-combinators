module Encode = Json_Encode
module Decode = Json_Decode

exception ParseError(string)

let decode: (Js.Json.t, Decode.t<'a>) => result<'a, string>

let parse: string => result<Js.Json.t, string>
let parseExn: string => Js.Json.t
