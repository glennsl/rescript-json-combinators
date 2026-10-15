import assert from "node:assert/strict"
import {test} from "node:test"
import * as Decode from "../src/Json_Decode.mjs"
import {Decode as Example} from "../examples/Recursive.mjs"

const tree = Decode.recursive(self => Decode.object(field => ({
  value: field.required("value", Decode.int),
  children: field.required("children", Decode.array(self)),
})))

test("recursive decodes nested branches and empty leaves", () => {
  const input = {value: 1, children: [
    {value: 2, children: [{value: 3, children: []}]},
    {value: 4, children: []},
  ]}
  const result = Decode.decode(input, tree)
  assert.equal(result.TAG, "Ok")
  assert.deepEqual(result._0, input)
})

test("recursive preserves nested field and array error paths", () => {
  const result = Decode.decode({value: 1, children: [
    {value: 2, children: [{value: "invalid", children: []}]},
  ]}, tree)
  assert.equal(result.TAG, "Error")
  assert.match(result._0, /Expected int/)
  assert.match(result._0, /field 'value'/)
  assert.equal((result._0.match(/field 'children'/g) ?? []).length, 2)
  assert.equal((result._0.match(/index 0/g) ?? []).length, 2)
})

test("recursive defers the builder until a value is decoded", () => {
  let builds = 0
  const decoder = Decode.recursive(() => {
    builds++
    return Decode.int
  })
  assert.equal(builds, 0)
  assert.equal(Decode.decode(42, decoder)._0, 42)
  assert.ok(builds > 0)
})

test("recursive example handles optional links", () => {
  const result = Decode.decode({value: 1, next: {value: 2}}, Example.list)
  assert.equal(result.TAG, "Ok")
  assert.equal(result._0.next.value, 2)
  assert.equal(result._0.next.next, undefined)
})
