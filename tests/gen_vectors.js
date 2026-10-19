// Generate test vectors from the reference implementation:
//   npm install lz-string@1.5.0 && node tests/gen_vectors.js > tests/vectors.json
const LZString = require("lz-string");

const units = (s) => Array.from({ length: s.length }, (_, i) => s.charCodeAt(i));

// Deterministic PRNG (mulberry32)
let seed = 42;
function rand() {
  seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
}
function randomString(len, alphabet) {
  let s = "";
  for (let i = 0; i < len; i++) s += alphabet[Math.floor(rand() * alphabet.length)];
  return s;
}

const inputs = [
  "", "a", "aa", "ab", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  "Hello, world!", "hello hello hello hello",
  "During tattooing, ink is injected into the skin, initiating an immune response, and cells called \"macrophages\" move into the area and \"eat up\" the ink. The macrophages carry some of the ink to the body's lymph nodes, but some that are filled with ink stay put, embedded in the skin. That's what makes the tattoo visible under the skin. Dalhousie Uiversity's Alec Falkenham is developing a topical cream that works by targeting the macrophages that have remained at the site of the tattoo. New macrophages move in to consume the previously pigment-filled macrophages and then migrate to the lymph nodes, eventually taking all the dye with them. \"When comparing it to laser-based tattoo removal, in which you see the burns, the scarring, the blisters, in this case, we've designed a drug that doesn't really have much off-target effect,\" he said. \"We're not targeting any of the normal skin cells, so you won't see a lot of inflammation. In fact, based on the process that we're actually using, we don't think there will be any inflammation at all and it would actually be anti-inflammatory.",
  "été, ça va ? Ünïcödé ÀÉÎÕÜ",
  "日本語のテキスト、日本語のテキスト。中文字符",
  "emoji 😀🎉🚀 and more 😀😀😀 𝔘𝔫𝔦𝔠𝔬𝔡𝔢",
  "\u0000\u0001\u0002ÿĀ￿",
  "tab\tnewline\ncr\r",
];
const alphabets = ["ab", "abcdefghij", "The quick brown fox jumps over the lazy dog ", "αβγδεζηθ日本😀"];
for (const a of alphabets)
  for (const len of [10, 100, 1000, 5000])
    inputs.push(randomString(len, Array.from(a)));
// All 8-bit and some 16-bit characters
let all = ""; for (let i = 0; i < 256; i++) all += String.fromCharCode(i);
for (let i = 256; i < 0xd800; i += 97) all += String.fromCharCode(i);
inputs.push(all, all + all);

const vectors = inputs.map((s) => ({
  input: units(s),
  raw: units(LZString.compress(s)),
  base64: LZString.compressToBase64(s),
  uri: LZString.compressToEncodedURIComponent(s),
  utf16: units(LZString.compressToUTF16(s)),
  uint8: Array.from(LZString.compressToUint8Array(s)),
}));
process.stdout.write(JSON.stringify(vectors) + "\n");
