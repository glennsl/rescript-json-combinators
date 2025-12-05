module Encode = Json_Encode
module Decode = Json_Decode

exception ParseError(string)

let decode: (JSON.t, Decode.t<'a>) => result<'a, string>

let parse: string => result<JSON.t, string>
let parseExn: string => JSON.t

@val external stringify: JSON.t => string = "JSON.stringify"
