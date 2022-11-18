# Underscore-Encoding

Constraints for the encoding scheme:

1. Common IDs with snake_case, camelCase, PascalCase,
    or SCREAMING_SNAKE_CASE must not be altered
1. Support all Unicode characters
1. Characters of the ASCII range must lead to shorter encodings

This encoding scheme is especially useful for GraphQL IDs,
which are constrained to the character set `[0-9A-Za-z_]`.


## Examples

Input | Output
------|-------
`camelCaseId` | `camelCaseId`
`snake_case_id` | `snake_case_id`
`id with spaces` | `id__0with__0spaces`
`id-with.special$chars!` | `id__Dwith__Especial__4chars__1`
`double__underscore` | `double__R__Runderscore`
`id_with_ümläutß` | `id_with___aaapmml__aaaoeut__aaanp`
`Emoji: 😅` | `Emoji__G__0__bpgaf`
`Multi Byte Emoji: 👨‍🦲` | `Multi__0Byte__0Emoji__G__0__bpegi__acaan__bpjlc`


## Explanation

The encoding scheme is based on the following rules:

1. All characters in `[0-9A-Za-z_]` except for `__` are encoded as is
1. `__` is encoded as `__R__R`
1. All other printable characters inside the ASCII range
    are encoded as a sequence of 3 characters: `__[0-9A-W]`
1. All other Unicode codepoints are encoded as a sequence of 7 characters:
    `__[a-p]{5}`, where the 5 characters are the hexadecimal representation
    with a modified hex alphabet ranging from `a` to `p` instead of `0` to `f`.

