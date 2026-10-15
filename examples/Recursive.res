type rec t = {value: int, next: option<t>}

module Decode = {
  open Json.Decode

  let list = recursive(self =>
    object(field => {
      value: field.required("value", int),
      next: field.optional("next", self),
    })
  )
}
