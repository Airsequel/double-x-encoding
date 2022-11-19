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
1. Optional support for encoding leading digits (like in `1_file_format`)
    to fulfill constraints of some ID schemes (e.g. GraphQL's).


## Examples

Input | Output
------|-------
`camelCaseId` | `camelCaseId`
`snake_case_id` | `snake_case_id`
`__Schema` | `__Schema`
`doxxing` | `doxxing`
`DOXXING` | `DOXXXXXXING`
`id with spaces` | `idXX0withXX0spaces`
`id-with.special$chars!` | `idXXDwithXXEspecialXX4charsXX1`
`id_with_ümläutß` | `id_with_XXaaapmmlXXaaaoeutXXaaanp`
`Emoji: 😅` | `EmojiXXGXX0XXbpgaf`
`Multi Byte Emoji: 👨‍🦲` | `MultiXX0ByteXX0EmojiXXGXX0XXbpegiXXacaanXXbpjlc`


## Explanation

The encoding scheme is based on the following rules:

1. All characters in `[0-9A-Za-z_]` except for `XX` are encoded as is
1. `XX` is encoded as `XXXXXX`
1. All other printable characters inside the ASCII range
    are encoded as a sequence of 3 characters: `XX[0-9A-W]`
1. All other Unicode codepoints are encoded as a sequence of 7 characters:
    `XX[a-p]{5}`, where the 5 characters are the hexadecimal representation
    with an alternative hex alphabet ranging from
    `a` to `p` instead of `0` to `f`.

If the optional leading digit encoding is enabled,
a leading digit is encoded as `XXZ[A-J]`, where `A` is `0` and `J` is `9`.
