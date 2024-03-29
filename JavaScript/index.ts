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
  "_": "R",
  "`": "S",

  "{": "T",
  "|": "U",
  "}": "V",
  "~": "W",

  // TODO: Remove this parsing workaround. Should be "X": "X"
  "X": "x",

  // "": "Z",  // Reserved for encoding digits
}

const charDecode = Object.fromEntries(
  Object
    .entries(charEncode)
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


export function doubleXEncode (
  str: string,
  options: {
    encodeLeadingDigit?: boolean,
    encodeDoubleUnderscore?: boolean,
  } = {},
): string {
  if (options.encodeLeadingDigit) {
    const firstChar = str.slice(0, 1)
    const firstCharEncoded = /^[0-9]$/.test(firstChar)
      ? "XXZ" + firstChar
      : firstChar

    options.encodeLeadingDigit = false

    return firstCharEncoded + doubleXEncode(str.slice(1), options)
  }

  let inNorm = str.replaceAll("XX", "XXXXXX")

  if (options.encodeDoubleUnderscore) {
    inNorm = inNorm.replaceAll("__", "XXRXXR")
  }

  let resultStr = ""

  for (const char of inNorm) {
    if (/[0-9A-Za-z_]/.test(char)) {
      resultStr += char
    }
    else {
      if (charEncode[char]) {
        resultStr += "XX" + charEncode[char]
      }
      else {
        const charHex = char
          .codePointAt(0)
          .toString(16)
        const encodedChar = charHex
          .split("")
          .map(char => hexShiftEncode[char] )
          .join("")

        if (charHex.length <= 5) {
          resultStr += "XX" + encodedChar.padStart(5, "a")
        }
        else if (charHex.length == 6) {
          resultStr += "XXY" + encodedChar.padStart(6, "a")
        }
        else {
          throw new Error("ERROR: Hex encoding is too long")
        }
      }
    }
  }

  return resultStr
}


export function doubleXDecode (str: string): string {
  // TODO: Remove this parsing workaround
  const strNorm = str.replaceAll("XXXXXX", "XXxXXx")

  return strNorm
    .split(/(XX[a-p]{5}|XX[0-9A-W]|XXY[a-p]{6}|XXZ[0-9]|XXx)/)
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
            ? word.slice(3, 4)
            : (word.slice(2, 3) == "Y")
              ? String.fromCodePoint(
                  parseInt(
                    word
                      .slice(3, 9)
                      .split("")
                      .map(char => hexShiftDecode[char])
                      .join(""),
                    16
                  )
                )
              : charDecode[word.slice(2, 3)]
        )
      : word
    )
    .join("")
}
