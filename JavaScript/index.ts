const charEncode = {
  " ": "0",
  "!": "1",
  '"': "2",
  "#": "3",
  "$": "4",
  "%": "5",
  "&": "6",
  "'": "7",
  "(": "8",
  ")": "9",
  "*": "A",
  "+": "B",
  ",": "C",
  "-": "D",
  ".": "E",
  "/": "F",

  ":": "G",
  ";": "H",
  "<": "I",
  "=": "J",
  ">": "K",
  "?": "L",
  "@": "M",

  "[": "N",
  "\\": "O",
  "]": "P",
  "^": "Q",
  // "_": "R",
  "`": "S",

  "{": "T",
  "|": "U",
  "}": "V",
  "~": "W",

  // TODO: Remove this parsing workaround. Should be "X": "X"
  "X": "Y",
}

const charDecode = Object.fromEntries(
  Object
    .entries(charEncode)
    .map(([k, v]) => [v, k])
)

const hexDigitEncode = {
  "0": "A",
  "1": "B",
  "2": "C",
  "3": "D",
  "4": "E",
  "5": "F",
  "6": "G",
  "7": "H",
  "8": "I",
  "9": "J",
}

const hexDigitDecode = Object.fromEntries(
  Object
    .entries(hexDigitEncode)
    .map(([k, v]) => [v, k])
)

const hexShiftEncode = {
  "0": "a",
  "1": "b",
  "2": "c",
  "3": "d",
  "4": "e",
  "5": "f",
  "6": "g",
  "7": "h",
  "8": "i",
  "9": "j",
  "a": "k",
  "b": "l",
  "c": "m",
  "d": "n",
  "e": "o",
  "f": "p",
}

const hexShiftDecode = Object.fromEntries(
  Object
    .entries(hexShiftEncode)
    .map(([k, v]) => [v, k])
)


export function doubleXEncode (str: string): string {
  const inNorm = str.replaceAll("XX", "XXXXXX")

  let resultStr = ""

  for (const char of inNorm) {
    if (/[0-9A-Za-z_]/.test(char)) {
      resultStr += char
    }
    else {
      const encodedChar =
              charEncode[char] ||
              char
                .codePointAt(0)
                .toString(16)
                .split("")
                .map(char => hexShiftEncode[char] )
                .join("")
                .padStart(5, "a") ||
              ""
      resultStr += "XX" + encodedChar
    }
  }

  return resultStr
}


export function doubleXEncodeLeadingDigit (str: string): string {
  const firstChar = str.slice(0, 1)
  const firstDigitHex = hexDigitEncode[firstChar]
  const firstCharEncoded = firstDigitHex
    ? "XXZ" + firstDigitHex
    : firstChar
  return firstCharEncoded + doubleXEncode(str.slice(1))
}


export function doubleXDecode (str: string): string {
  // TODO: Remove this workaround to simplify parsing
  const strNorm = str.replaceAll("XXXXXX", "XXYXXY")

  return strNorm
    .split(/(XXY|XXZ[A-J]|XX[0-9A-W]|XX[a-p]{5})/)
    .filter(Boolean)  // Remove empty strings
    .map(word =>
      word.startsWith("XX")
      ? (word.slice(2, 3) >= "a" && word.slice(2, 3) <= "p"
          ? String.fromCodePoint(
              parseInt(
                word
                  .slice(2, 7)
                  .split("")
                  .map(char => hexShiftDecode[char])
                  .join(""),
                16
              )
            )
          : (word.slice(2, 3) == "Z")
            ? hexDigitDecode[word.slice(3, 4)]
            : charDecode[word.slice(2, 3)]
        )
      : word
    )
    .join("")
}
