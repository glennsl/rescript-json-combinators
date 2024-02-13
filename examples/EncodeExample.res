// Encoding JSON data structures using Json.Encode

// Encode string array, fast because `stringArray` only changes the type
// prints ["foo", "bar"]
let _ = ["foo", "bar"]->Json.Encode.stringArray->Js.log

// Encode string array, slower but more flexible
// prints ["foo", "bar"]
let _ = ["foo", "bar"]->Json.Encode.array(Json.Encode.string)(_)->Js.log

// Encode object, fast but unsafe because it cannot guarantee that it only contains valid JSON
// prints { x: 42, foo: 'bar' }
let _ = Json.Encode.Unsafe.object({
  "x": Json.Encode.int(42),
  "foo": Json.Encode.string("bar"),
})->Js.log

// Encode object, slower but safe
// prints { x: 42, foo: 'bar' }
let _ = Json.Encode.object([("x", Json.Encode.int(42)), ("foo", Json.Encode.string("bar"))])->Js.log

//Advanced example: encode a record

type point = {
  x: float,
  y: float,
}

type line = {
  points: array<point>,
  thickness: option<int>,
}

module Encode = {
  open! Json.Encode

  let point = r =>
    Unsafe.object({
      "x": float(r.x),
      "y": float(r.y),
    })

  let line = (~points, ~thickness=?, ()) =>
    Unsafe.object({
      "points": array(point)(points),
      "thickness": option(int)(thickness),
    })
}

let _ = Encode.line(~points=[{x: 1.1, y: -0.4}, {x: 5.3, y: 3.8}], ~thickness=2, ())->Js.log
