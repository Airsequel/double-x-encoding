import listOfNaughtyStrings from "npm:blns"
import {
  doubleXEncode,
  doubleXDecode,
  doubleXEncodeLeadingDigit,
} from "./index.ts"

let correctConversions = 0
let correctDigitConversions = 0

listOfNaughtyStrings.forEach(str => {
  console.info("===============================================\n")
  console.info("original:\t", str)

  const encoded = doubleXEncode(str)
  const decoded = doubleXDecode(encoded)
  console.info("encoded:\t", encoded)
  console.info("decoded:\t", decoded)
  if (str !== decoded ) {
    throw new Error(
      `ERROR: Decoded string "${decoded
      }" is not equal to original string "${str}"`
    )
  }
  else {
    correctConversions += 1
  }

  const encodedDigit = doubleXEncodeLeadingDigit(str)
  const decodedDigit = doubleXDecode(encodedDigit)
  console.info("encoded leading digit:\t", encodedDigit)
  console.info("decoded leadingt digit:\t", decodedDigit)
  if (str !== decodedDigit ) {
    throw new Error(
      `ERROR: Decoded string "${decodedDigit
      }" is not equal to original string "${str}"`
    )
  }
  else {
    correctDigitConversions += 1
  }

  console.log("\x1Bc")
})

console.info("===============================================\n")

console.info(`
  Correct conversions: ${
    correctConversions}/${listOfNaughtyStrings.length}
  Correct Leading Digit conversions: ${
    correctDigitConversions}/${listOfNaughtyStrings.length}
`)
