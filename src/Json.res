module Encode = Json_Encode
module Decode = Json_Decode

exception ParseError(string)

let decode = Decode.decode

let parse = str =>
  try Ok(str->Js.Json.parseExn) catch {
  | ex =>
    let message = ex->JsExn.fromException->Option.flatMap(JsExn.message)->Option.getOr("Unknown error")
    Error(message)
  }

let parseExn = str =>
  try str->Js.Json.parseExn catch {
  | ex =>
    let message = ex->JsExn.fromException->Option.flatMap(JsExn.message)->Option.getOr("Unknown error")
    throw(ParseError(message))
  }

@val external stringify: Js.Json.t => string = "JSON.stringify"
