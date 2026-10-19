import Lean.Data.Json
import LzString

open Lean LzString

instance : FromJson UInt16 where
  fromJson? j := return (← j.getNat?).toUInt16

instance : FromJson UInt8 where
  fromJson? j := return (← j.getNat?).toUInt8

structure TestVector where
  input : Array UInt16
  raw : Array UInt16
  base64 : String
  uri : String
  utf16 : Array UInt16
  uint8 : Array UInt8
  deriving FromJson

def check (failures : IO.Ref Nat) (name : String) (ok : Bool) : IO Unit := do
  unless ok do
    failures.modify (· + 1)
    IO.eprintln s!"FAIL: {name}"

def main : IO UInt32 := do
  let json ← IO.ofExcept <| Json.parse (← IO.FS.readFile "tests/vectors.json")
  let vectors : Array TestVector ← IO.ofExcept <| fromJson? json
  let failures ← IO.mkRef 0
  for v in vectors, i in [0:vectors.size] do
    let some input := fromUTF16 v.input | throw <| .userError s!"vector {i}: invalid input"
    let some utf16 := fromUTF16 v.utf16 | throw <| .userError s!"vector {i}: invalid utf16"
    let uint8 := ByteArray.mk v.uint8
    let check name ok := check failures s!"vector {i}: {name}" ok
    check "compress" (compress input == v.raw)
    check "compressToBase64" (compressToBase64 input == v.base64)
    check "compressToEncodedURIComponent" (compressToEncodedURIComponent input == v.uri)
    check "compressToUTF16" (compressToUTF16 input == utf16)
    check "compressToUint8Array" ((compressToUint8Array input).data == uint8.data)
    check "decompress" (decompress v.raw == some input)
    check "decompressFromBase64" (decompressFromBase64 v.base64 == some input)
    check "decompressFromEncodedURIComponent" (decompressFromEncodedURIComponent v.uri == some input)
    check "decompressFromUTF16" (decompressFromUTF16 utf16 == some input)
    check "decompressFromUint8Array" (decompressFromUint8Array uint8 == some input)
    -- Spaces in URI-encoded input are read as '+'.
    check "decompressFromEncodedURIComponent (spaces)"
      (decompressFromEncodedURIComponent (v.uri.replace "+" " ") == some input)
  -- Same conventions as the JavaScript library for empty input.
  check failures "empty decompress" (decompress #[] == none)
  check failures "empty decompressFromBase64" (decompressFromBase64 "" == none)
  check failures "empty decompressFromUTF16" (decompressFromUTF16 "" == none)
  check failures "empty decompressFromEncodedURIComponent" (decompressFromEncodedURIComponent "" == none)
  check failures "empty decompressFromUint8Array" (decompressFromUint8Array .empty == none)
  let n ← failures.get
  if n == 0 then
    IO.println s!"All {vectors.size} vectors passed."
    return 0
  else
    IO.eprintln s!"{n} failure(s)"
    return 1
