type t<'a> = JSON.t => 'a

type fieldDecoders = {
  optional: 'a. (string, t<'a>) => option<'a>,
  required: 'a. (string, t<'a>) => 'a,
}

exception DecodeError(string)

module Error = {
  let expected = (kind, json) => throw(DecodeError(`Expected ${kind}, got ${JSON.stringify(json)}`))
}

let custom = f => f

let id = json => json

let float = json => {
  if typeof(json) != #number {
    Error.expected("float", json)
  }

  Obj.magic(json)
}

let int = json => {
  if typeof(json) != #number {
    Error.expected("int", json)
  }

  let truncated: float = %raw("json | 0")
  let num: float = Obj.magic(json)
  if truncated != num || !Float.isFinite(num) {
    Error.expected("int", json)
  }

  Obj.magic(json)
}

let bool = json => {
  if typeof(json) != #boolean {
    Error.expected("bool", json)
  }

  Obj.magic(json)
}

let string = json => {
  if typeof(json) != #string {
    Error.expected("string", json)
  }

  Obj.magic(json)
}

let array = decode =>
  json => {
    if !Array.isArray(json) {
      Error.expected("array", json)
    }

    let source: array<JSON.t> = Obj.magic(json)
    let target = %raw("new Array(json.length)")

    for i in 0 to Array.length(source) - 1 {
      try {
        let value = decode(%raw("json[i]"))
        target->Array.setUnsafe(i, value)
      } catch {
      | DecodeError(msg) => throw(DecodeError(`${msg}\n\tin array at index ${Int.toString(i)}`))
      }
    }

    target
  }

let list = decode => json => array(decode)(json)->List.fromArray

let option = decode =>
  json => {
    if Obj.magic(json) == null {
      None
    } else {
      Some(decode(json))
    }
  }

let date = json => string(json)->Date.fromString

let tuple2 = (decodeA, decodeB) =>
  json => {
    if !Array.isArray(json) {
      Error.expected("array", json)
    }

    let arr: array<JSON.t> = Obj.magic(json)
    if Array.length(arr) != 2 {
      throw(
        DecodeError(
          `Expected array of length 2, got array of length ${Array.length(arr)->Int.toString}`,
        ),
      )
    }

    try (decodeA(arr->Array.getUnsafe(0)), decodeB(arr->Array.getUnsafe(1))) catch {
    | DecodeError(msg) => throw(DecodeError(`${msg}\n\tin pair`))
    }
  }
let pair = tuple2

let tuple3 = (decodeA, decodeB, decodeC) =>
  json => {
    if !Array.isArray(json) {
      Error.expected("array", json)
    }

    let arr: array<JSON.t> = Obj.magic(json)
    if Array.length(arr) != 3 {
      throw(
        DecodeError(
          `Expected array of length 3, got array of length ${Array.length(arr)->Int.toString}`,
        ),
      )
    }

    try (
      decodeA(arr->Array.getUnsafe(0)),
      decodeB(arr->Array.getUnsafe(1)),
      decodeC(arr->Array.getUnsafe(2)),
    ) catch {
    | DecodeError(msg) => throw(DecodeError(`${msg}\n\tin pair`))
    }
  }

let tuple4 = (decodeA, decodeB, decodeC, decodeD) =>
  json => {
    if !Array.isArray(json) {
      Error.expected("array", json)
    }

    let arr: array<JSON.t> = Obj.magic(json)
    if Array.length(arr) != 4 {
      throw(
        DecodeError(
          `Expected array of length 4, got array of length ${Array.length(arr)->Int.toString}`,
        ),
      )
    }

    try (
      decodeA(arr->Array.getUnsafe(0)),
      decodeB(arr->Array.getUnsafe(1)),
      decodeC(arr->Array.getUnsafe(2)),
      decodeD(arr->Array.getUnsafe(3)),
    ) catch {
    | DecodeError(msg) => throw(DecodeError(`${msg}\n\tin pair`))
    }
  }

let dict = decode =>
  json => {
    if typeof(json) != #object || Array.isArray(json) || Obj.magic(json) == null {
      Error.expected("object", json)
    }

    let source: Dict.t<JSON.t> = Obj.magic(json)
    try source->Dict.mapValues(decode)->Obj.magic catch {
    | DecodeError(msg) => throw(DecodeError(`${msg}\n\tin dict'`))
    }
  }

let field = (key, decode) =>
  json => {
    if typeof(json) != #object || Array.isArray(json) || Obj.magic(json) == null {
      Error.expected("object", json)
    }

    if !(%raw("key in json")) {
      throw(DecodeError(`${key} required`))
    }

    try decode(%raw("json[key]")) catch {
    | DecodeError(msg) => throw(DecodeError(`${msg}\n\tat field '${key}'`))
    }
  }

let object = f =>
  json => {
    if typeof(json) != #object || Array.isArray(json) || Obj.magic(json) == null {
      throw(Error.expected("object", json))
    }

    let optional = (key, decode) => {
      if !(%raw("key in json")) {
        None
      } else {
        try {
          let value = decode(%raw("json[key]"))
          Some(value)
        } catch {
        | DecodeError(msg) => throw(DecodeError(`${msg}\n\tat field '${key}'`))
        }
      }
    }

    let required = (key, decode) => {
      if !(%raw("key in json")) {
        throw(DecodeError(`${key} required`))
      }

      try decode(%raw("json[key]")) catch {
      | DecodeError(msg) => throw(DecodeError(`${msg}\n\tat field '${key}'`))
      }
    }

    f({optional, required})
  }

let oneOf = decoders =>
  json => {
    let errors = []

    let rec loop = i => {
      if i >= Array.length(decoders) {
        throw(
          DecodeError(
            `All decoders given to oneOf failed. Here are all the errors:\n- ${errors->Array.join(
                "\n",
              )}\nAnd the JSON being decoded: ${JSON.stringify(json)}`,
          ),
        )
      }

      let decode = Array.getUnsafe(decoders, i)
      try decode(json) catch {
      | DecodeError(err) =>
        errors->Array.push(err)->ignore
        loop(i + 1)
      }
    }

    loop(0)
  }

let map = (decode, f) => json => f(decode(json))

let flatMap = (decodeA, f) =>
  json => {
    let decodeB = f(decodeA(json))
    decodeB(json)
  }

let indirect = f => json => f()(json)

let decode = (json, decode) =>
  try Ok(decode(json)) catch {
  | DecodeError(msg) => Error(msg)
  }
