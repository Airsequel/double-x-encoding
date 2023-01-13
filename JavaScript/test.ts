import listOfNaughtyStrings from "npm:blns"
import {
  doubleXEncode,
  doubleXDecode,
} from "./index.ts"

let correctConversions = 0
let correctDigitConversions = 0
let correctDblUnderConversions = 0

// Support characters from the Supplementary Private Use Area-B
const extraStrings = ["\u{100000}", "\u{10FFFF}"]
const allStrings = listOfNaughtyStrings.concat(extraStrings)

allStrings.forEach((str: string) => {
  const encoded = doubleXEncode(str)
  const decoded = doubleXDecode(encoded)
  if (str !== decoded ) {
    throw new Error(
      `ERROR: Decoded string "${decoded
      }" is not equal to original string "${str}"`
    )
  }
  else {
    correctConversions += 1
  }

  const encodedDigit = doubleXEncode(str, { encodeLeadingDigit: true })
  const decodedDigit = doubleXDecode(encodedDigit)
  if (str !== decodedDigit ) {
    throw new Error(
      `ERROR: Decoded string "${decodedDigit
      }" is not equal to original string "${str}"`
    )
  }
  else {
    correctDigitConversions += 1
  }

  const encodedDblUnder = doubleXEncode(str, { encodeDoubleUnderscore: true })
  const decodedDblUnder = doubleXDecode(encodedDblUnder)
  if (str !== decodedDblUnder ) {
    throw new Error(
      `ERROR: Decoded string "${decodedDblUnder
      }" is not equal to original string "${str}"`
    )
  }
  else {
    correctDblUnderConversions += 1
  }
})

console.info(`
  Correct conversions: ${
    correctConversions}/${allStrings.length}
  Correct Leading Digit conversions: ${
    correctDigitConversions}/${allStrings.length}
  Correct Leading Digit conversions: ${
    correctDblUnderConversions}/${allStrings.length}
`)
