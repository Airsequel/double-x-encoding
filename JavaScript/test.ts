import listOfNaughtyStrings from "npm:blns"
import { underscoreEncode, underscoreDecode } from "./index.ts"

let correctConversions = 0

listOfNaughtyStrings.forEach(str => {
  const encoded = underscoreEncode(str)
  const decoded = underscoreDecode(encoded)
  console.info("===============================================\n")
  console.info("original:\t", str)
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
