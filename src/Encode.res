external int: int => Js.Json.t = "%identity"
external float: float => Js.Json.t = "%identity"
external bool: bool => Js.Json.t = "%identity"
external string: string => Js.Json.t = "%identity"

module Unsafe = {
  external object: {..} => Js.Json.t = "%identity"
}
