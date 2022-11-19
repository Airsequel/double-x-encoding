import listOfNaughtyStrings from "npm:blns"
import { doubleXEncode, doubleXDecode } from "./index.ts"

let correctConversions = 0

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
  console.log("\x1Bc")
})

console.info("===============================================\n")

console.info(
  `Correct conversions: ${correctConversions}/${listOfNaughtyStrings.length}`
)
