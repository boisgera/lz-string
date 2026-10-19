import Std.Data.HashMap
import Std.Data.HashSet

/-!
# lz-string

A port of [lz-string](https://github.com/pieroxy/lz-string) (version 1.5.0).

The JavaScript library works on UTF-16 code units, so the core algorithms here
operate on `Array UInt16`. Lean strings are converted to and from UTF-16 at the
boundaries, which makes the output bit-for-bit identical to the JavaScript one,
including for characters outside the Basic Multilingual Plane.

Where the JavaScript functions return `null` (empty or corrupt input), the Lean
functions return `none`. They also return `none` when the decompressed data is
not valid UTF-16, and for streams starting with the invalid code 3, on which the
JavaScript implementation misbehaves (it returns text containing `"undefined"`,
or throws).
-/

namespace LzString

/-! ## UTF-16 conversions -/

/-- Encode a string as UTF-16 code units (what JavaScript's `charCodeAt` sees). -/
def toUTF16 (s : String) : Array UInt16 :=
  s.foldl (init := #[]) fun acc c =>
    let v := c.toNat
    if v < 0x10000 then
      acc.push v.toUInt16
    else
      let u := v - 0x10000
      (acc.push (0xD800 + u / 0x400).toUInt16).push (0xDC00 + u % 0x400).toUInt16

/-- Decode UTF-16 code units. Fails on unpaired surrogates. -/
def fromUTF16 (units : Array UInt16) : Option String := Id.run do
  let mut out := ""
  let mut i := 0
  while i < units.size do
    let u := units[i]!.toNat
    if u < 0xD800 || u > 0xDFFF then
      out := out.push (Char.ofNat u)
      i := i + 1
    else if u ≤ 0xDBFF && i + 1 < units.size then
      let l := units[i + 1]!.toNat
      if l < 0xDC00 || l > 0xDFFF then return none
      out := out.push (Char.ofNat (0x10000 + (u - 0xD800) * 0x400 + (l - 0xDC00)))
      i := i + 2
    else
      return none
  return some out

/-! ## Compression -/

/-- Packs bits, most significant first, into values of `bitsPerChar` bits. -/
private structure BitWriter where
  bitsPerChar : Nat
  data : Array Nat := #[]
  val : Nat := 0
  position : Nat := 0

private def BitWriter.writeBit (w : BitWriter) (bit : Nat) : BitWriter :=
  let val := (w.val <<< 1) ||| bit
  if w.position == w.bitsPerChar - 1 then
    { w with data := w.data.push val, val := 0, position := 0 }
  else
    { w with val, position := w.position + 1 }

/-- Write the `n` low bits of `value`, least significant first. -/
private def BitWriter.writeBits (w : BitWriter) (value n : Nat) : BitWriter := Id.run do
  let mut w := w
  let mut value := value
  for _ in [0:n] do
    w := w.writeBit (value &&& 1)
    value := value >>> 1
  return w

/-- Pad the last value with zeros and return all values. -/
private def BitWriter.flush (w : BitWriter) : Array Nat := Id.run do
  let mut val := w.val
  let mut position := w.position
  for _ in [0:w.bitsPerChar] do
    val := val <<< 1
    if position == w.bitsPerChar - 1 then break
    position := position + 1
  return w.data.push val

private structure CompressState where
  out : BitWriter
  /-- Single characters whose literal value has not been emitted yet. -/
  toCreate : Std.HashSet UInt16 := {}
  enlargeIn : Nat := 2  -- Compensate for the first entry which should not count
  numBits : Nat := 2

private def CompressState.decrEnlargeIn (s : CompressState) : CompressState :=
  if s.enlargeIn == 1 then
    { s with enlargeIn := 2 ^ s.numBits, numBits := s.numBits + 1 }
  else
    { s with enlargeIn := s.enlargeIn - 1 }

/-- The current phrase `w`: its dictionary code, and its character when it has length 1. -/
private structure Phrase where
  code : Nat
  single : Option UInt16

/-- Emit the code for the phrase `w` (preceded by a literal if it is a new character). -/
private def CompressState.emit (s : CompressState) (w : Phrase) : CompressState :=
  let s := match w.single with
    | some c =>
      if s.toCreate.contains c then
        let out :=
          if c < 256 then
            (s.out.writeBits 0 s.numBits).writeBits c.toNat 8
          else
            (s.out.writeBits 1 s.numBits).writeBits c.toNat 16
        { s with out, toCreate := s.toCreate.erase c }.decrEnlargeIn
      else
        { s with out := s.out.writeBits w.code s.numBits }
    | none => { s with out := s.out.writeBits w.code s.numBits }
  s.decrEnlargeIn

/--
The core LZ compressor. Returns values of `bitsPerChar` bits each
(`1 ≤ bitsPerChar ≤ 16`).
-/
def compressCore (input : Array UInt16) (bitsPerChar : Nat) : Array Nat := Id.run do
  -- The dictionary is a trie: single characters, plus (phrase code, next char) pairs.
  let mut chars : Std.HashMap UInt16 Nat := {}
  let mut pairs : Std.HashMap (Nat × UInt16) Nat := {}
  let mut dictSize := 3
  let mut s : CompressState := { out := { bitsPerChar } }
  let mut w : Option Phrase := none
  for c in input do
    let cCode ← match chars[c]? with
      | some code => pure code
      | none =>
        chars := chars.insert c dictSize
        s := { s with toCreate := s.toCreate.insert c }
        dictSize := dictSize + 1
        pure (dictSize - 1)
    match w with
    | none => w := some ⟨cCode, some c⟩
    | some phrase =>
      match pairs[(phrase.code, c)]? with
      | some code => w := some ⟨code, none⟩
      | none =>
        s := s.emit phrase
        pairs := pairs.insert (phrase.code, c) dictSize
        dictSize := dictSize + 1
        w := some ⟨cCode, some c⟩
  if let some phrase := w then
    s := s.emit phrase
  -- Mark the end of the stream
  return (s.out.writeBits 2 s.numBits).flush

/-! ## Decompression -/

/-- Reads bits from values of which only the bits below `resetValue` are significant. -/
private structure BitReader where
  resetValue : UInt32
  getNext : Nat → UInt32
  val : UInt32
  position : UInt32
  index : Nat

/-- Read `n` bits, least significant first. -/
private def BitReader.readBits (r : BitReader) (n : Nat) : Nat × BitReader := Id.run do
  let mut r := r
  let mut bits := 0
  for i in [0:n] do
    let resb := r.val &&& r.position
    r := { r with position := r.position >>> 1 }
    if r.position == 0 then
      r := { r with position := r.resetValue, val := r.getNext r.index, index := r.index + 1 }
    if resb != 0 then
      bits := bits ||| (1 <<< i)
  return (bits, r)

/--
The core LZ decompressor. `length` is the number of input values, each of which
carries `log2 resetValue + 1` bits, fetched with `getNext`.
-/
def decompressCore (length : Nat) (resetValue : UInt32) (getNext : Nat → UInt32) :
    Option (Array UInt16) := Id.run do
  let mut r : BitReader :=
    { resetValue, getNext, val := getNext 0, position := resetValue, index := 1 }
  -- Codes 0, 1 and 2 are reserved (8-bit literal, 16-bit literal, end of stream).
  let mut dictionary : Array (Array UInt16) := #[#[], #[], #[]]
  let mut enlargeIn := 4
  let mut numBits := 3
  let (next, r') := r.readBits 2
  r := r'
  let c ← match next with
    | 0 => let (bits, r') := r.readBits 8; r := r'; pure bits
    | 1 => let (bits, r') := r.readBits 16; r := r'; pure bits
    | 2 => return some #[]
    | _ => return none  -- the JavaScript implementation goes astray here
  let mut w : Array UInt16 := #[c.toUInt16]
  dictionary := dictionary.push w
  let mut result := w
  repeat
    if r.index > length then return some #[]
    let (bits, r') := r.readBits numBits
    r := r'
    let mut code := bits
    match bits with
    | 0 | 1 =>
      let (lit, r') := r.readBits (if bits == 0 then 8 else 16)
      r := r'
      dictionary := dictionary.push #[lit.toUInt16]
      code := dictionary.size - 1
      enlargeIn := enlargeIn - 1
    | 2 => return some result
    | _ => pure ()
    if enlargeIn == 0 then
      enlargeIn := 2 ^ numBits
      numBits := numBits + 1
    let entry ←
      if h : code < dictionary.size then pure dictionary[code]
      else if code == dictionary.size then pure (w.push w[0]!)
      else return none
    result := result ++ entry
    -- Add w + entry[0] to the dictionary.
    dictionary := dictionary.push (w.push entry[0]!)
    enlargeIn := enlargeIn - 1
    w := entry
    if enlargeIn == 0 then
      enlargeIn := 2 ^ numBits
      numBits := numBits + 1
  return none  -- unreachable

/-! ## Public API -/

private def keyStrBase64 : Array Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=".toList.toArray

private def keyStrUriSafe : Array Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-$".toList.toArray

/-- Value of a UTF-16 code unit in `alphabet`; unknown characters (and padding past the end) count as 0. -/
private def baseValue (alphabet : Array Char) (unit : Option UInt16) : UInt32 :=
  match unit with
  | some u => (alphabet.findIdx? (·.val == u.toUInt32)).getD 0 |>.toUInt32
  | none => 0

private def encode (alphabet : Array Char) (values : Array Nat) : String :=
  values.foldl (init := "") fun s v => s.push alphabet[v]!

/-- Compress to raw 16-bit values (the equivalent of `LZString.compress`).

The result is not a `String` because it may contain unpaired surrogates. -/
def compress (input : String) : Array UInt16 :=
  (compressCore (toUTF16 input) 16).map (·.toUInt16)

/-- Inverse of `compress`. -/
def decompress (compressed : Array UInt16) : Option String := do
  if compressed.isEmpty then none
  fromUTF16 (← decompressCore compressed.size 32768 fun i => (compressed[i]?.getD 0).toUInt32)

/-- Compress to a valid Base64 string. -/
def compressToBase64 (input : String) : String :=
  let res := encode keyStrBase64 (compressCore (toUTF16 input) 6)
  match res.length % 4 with
  | 1 => res ++ "==="
  | 2 => res ++ "=="
  | 3 => res ++ "="
  | _ => res

/-- Inverse of `compressToBase64`. -/
def decompressFromBase64 (input : String) : Option String := do
  if input.isEmpty then none
  let units := toUTF16 input
  fromUTF16 (← decompressCore units.size 32 fun i => baseValue keyStrBase64 units[i]?)

/-- Compress to a string of valid UTF-16 characters (15 bits per character). -/
def compressToUTF16 (input : String) : String :=
  (compressCore (toUTF16 input) 15).foldl (init := "") (fun s v => s.push (Char.ofNat (v + 32)))
    |>.push ' '

/-- Inverse of `compressToUTF16`. -/
def decompressFromUTF16 (compressed : String) : Option String := do
  if compressed.isEmpty then none
  let units := toUTF16 compressed
  fromUTF16 (← decompressCore units.size 16384 fun i =>
    match units[i]? with
    | some u => u.toUInt32 - 32  -- wraps like JavaScript's 32-bit bitwise operators
    | none => 0)

/-- Compress to bytes (UCS-2, big endian). -/
def compressToUint8Array (input : String) : ByteArray :=
  (compress input).foldl (init := .empty) fun b (v : UInt16) => (b.push (v >>> 8).toUInt8).push v.toUInt8

/-- Inverse of `compressToUint8Array`. Fails on an odd number of bytes. -/
def decompressFromUint8Array (compressed : ByteArray) : Option String := do
  if compressed.size % 2 != 0 then none
  let units := Array.ofFn (n := compressed.size / 2) fun ⟨i, _⟩ =>
    compressed[2 * i]!.toUInt16 <<< 8 ||| compressed[2 * i + 1]!.toUInt16
  decompress units

/-- Compress to a string that is safe to use in a URI component without escaping. -/
def compressToEncodedURIComponent (input : String) : String :=
  encode keyStrUriSafe (compressCore (toUTF16 input) 6)

/-- Inverse of `compressToEncodedURIComponent`. Spaces are read as `+`. -/
def decompressFromEncodedURIComponent (input : String) : Option String := do
  if input.isEmpty then none
  let units := toUTF16 (input.replace " " "+")
  fromUTF16 (← decompressCore units.size 32 fun i => baseValue keyStrUriSafe units[i]?)

end LzString
