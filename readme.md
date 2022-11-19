# Double X Encoding

Encoding scheme to encode any Unicode string
with only characters from `[0-9a-zA-Z_]`.
Therefore it's quite similar to URL percent-encoding.
It's especially useful for GraphQL ID generation.

Constraints for the encoding scheme:

1. Common IDs like `file_format`, `fileFormat`, `FileFormat`,
    `FILE_FORMAT`, `__file_format__`, … must not be altered
1. Support all Unicode characters
1. Characters of the ASCII range must lead to shorter encodings


## Examples

Input | Output
------|-------
`camelCaseId` | `camelCaseId`
`snake_case_id` | `snake_case_id`
`__Schema` | `__Schema`
`doxxing` | `doxxXxxXing`
`id with spaces` | `idxx0withxx0spaces`
`id-with.special$chars!` | `idxxDwithxxEspecialxx4charsxx1`
`id_with_ümläutß` | `id_with_xxaaapmmlxxaaaoeutxxaaanp`
`Emoji: 😅` | `EmojixxGxx0xxbpgaf`
`Multi Byte Emoji: 👨‍🦲` | `Multixx0Bytexx0EmojixxGxx0xxbpegixxacaanxxbpjlc`


## Explanation

The encoding scheme is based on the following rules:

1. All characters in `[0-9A-Za-z_]` except for `xx` are encoded as is
1. `xx` is encoded as `xxXxxX`
1. All other printable characters inside the ASCII range
    are encoded as a sequence of 3 characters: `xx[0-9A-W]`
1. All other Unicode codepoints are encoded as a sequence of 7 characters:
    `xx[a-p]{5}`, where the 5 characters are the hexadecimal representation
    with an alternative hex alphabet ranging from
    `a` to `p` instead of `0` to `f`.

