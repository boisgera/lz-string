import LzString

open LzString

def usage : String := "usage: lz-string (compress | decompress) [--base64 | --uri | --utf16 | --bytes]

Reads from stdin and writes to stdout. The default format is --base64.
--bytes is the UCS-2 big endian encoding of `compress` (compressToUint8Array)."

def main (args : List String) : IO UInt32 := do
  let (mode, format) ← match args with
    | [mode] => pure (mode, "--base64")
    | [mode, format] => pure (mode, format)
    | _ => IO.eprintln usage; return 2
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  match mode, format with
  | "compress", _ =>
    let some input := String.fromUTF8? (← stdin.readBinToEnd)
      | IO.eprintln "lz-string: input is not valid UTF-8"; return 1
    match format with
    | "--bytes" => stdout.write (compressToUint8Array input)
    | "--base64" => stdout.putStr (compressToBase64 input)
    | "--uri" => stdout.putStr (compressToEncodedURIComponent input)
    | "--utf16" => stdout.putStr (compressToUTF16 input)
    | _ => IO.eprintln usage; return 2
  | "decompress", _ =>
    let result ← match format with
      | "--bytes" => pure (decompressFromUint8Array (← stdin.readBinToEnd))
      | "--base64" => pure (decompressFromBase64 (← stdin.readToEnd).trimAscii.toString)
      | "--uri" => pure (decompressFromEncodedURIComponent (← stdin.readToEnd).trimAscii.toString)
      | "--utf16" => pure (decompressFromUTF16 (← stdin.readToEnd))
      | _ => IO.eprintln usage; return 2
    let some output := result | IO.eprintln "lz-string: invalid compressed data"; return 1
    stdout.putStr output
  | _, _ => IO.eprintln usage; return 2
  return 0
