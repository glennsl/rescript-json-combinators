module Encode = Json_Encode
module Decode = Json_Decode

exception ParseError(string)

let decode = Decode.decode

let parse = str =>
  try Ok(str->JSON.parseOrThrow) catch {
  | ex =>
    let message =
      ex->JsExn.fromException->Option.flatMap(JsExn.message)->Option.getOr("Unknown error")
    Error(message)
  }

let parseExn = str =>
  try str->JSON.parseOrThrow catch {
  | ex =>
    let message =
      ex->JsExn.fromException->Option.flatMap(JsExn.message)->Option.getOr("Unknown error")
    throw(ParseError(message))
  }

@val external stringify: JSON.t => string = "JSON.stringify"
