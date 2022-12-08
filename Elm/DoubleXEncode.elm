module DoubleXEncode exposing (..)

{-| Implementation of double-X-encoder and -decoder in Haskell
-}

import String exposing (replace)
import UInt64
import UInt64.Digits



-- charEncode mapping in Elm


charEncode : Char -> Char
charEncode char =
    case char of
        ' ' ->
            '0'

        '!' ->
            '1'

        '"' ->
            '2'

        '#' ->
            '3'

        '$' ->
            '4'

        '%' ->
            '5'

        '&' ->
            '6'

        '\'' ->
            '7'

        '(' ->
            '8'

        ')' ->
            '9'

        '*' ->
            'A'

        '+' ->
            'B'

        ',' ->
            'C'

        '-' ->
            'D'

        '.' ->
            'E'

        '/' ->
            'F'

        ':' ->
            'G'

        ';' ->
            'H'

        '<' ->
            'I'

        '=' ->
            'J'

        '>' ->
            'K'

        '?' ->
            'L'

        '@' ->
            'M'

        '[' ->
            'N'

        '\\' ->
            'O'

        ']' ->
            'P'

        '^' ->
            'Q'

        '_' ->
            'R'

        '`' ->
            'S'

        '{' ->
            'T'

        '|' ->
            'U'

        '}' ->
            'V'

        '~' ->
            'W'

        -- TODO: Remove this parsing workaround. Should be "X": "X"
        'X' ->
            'Y'

        -- '': 'Z',  -- Reserved for encoding digits
        _ ->
            '\u{0000}'


charDecode : Char -> Char
charDecode char =
    case char of
        '0' ->
            ' '

        '1' ->
            '!'

        '2' ->
            '"'

        '3' ->
            '#'

        '4' ->
            '$'

        '5' ->
            '%'

        '6' ->
            '&'

        '7' ->
            '\''

        '8' ->
            '('

        '9' ->
            ')'

        'A' ->
            '*'

        'B' ->
            '+'

        'C' ->
            ','

        'D' ->
            '-'

        'E' ->
            '.'

        'F' ->
            '/'

        'G' ->
            ':'

        'H' ->
            ';'

        'I' ->
            '<'

        'J' ->
            '='

        'K' ->
            '>'

        'L' ->
            '?'

        'M' ->
            '@'

        'N' ->
            '['

        'O' ->
            '\\'

        'P' ->
            ']'

        'Q' ->
            '^'

        'R' ->
            '_'

        'S' ->
            '`'

        'T' ->
            '{'

        'U' ->
            '|'

        'V' ->
            '}'

        'W' ->
            '~'

        'Y' ->
            'X'

        _ ->
            '\u{0000}'


hexShiftEncode : Char -> Char
hexShiftEncode digit =
    case digit of
        '0' ->
            'a'

        '1' ->
            'b'

        '2' ->
            'c'

        '3' ->
            'd'

        '4' ->
            'e'

        '5' ->
            'f'

        '6' ->
            'g'

        '7' ->
            'h'

        '8' ->
            'i'

        '9' ->
            'j'

        'a' ->
            'k'

        'b' ->
            'l'

        'c' ->
            'm'

        'd' ->
            'n'

        'e' ->
            'o'

        'f' ->
            'p'

        _ ->
            '\u{0000}'


hexShiftDecode : Char -> Char
hexShiftDecode digit =
    case digit of
        'a' ->
            '0'

        'b' ->
            '1'

        'c' ->
            '2'

        'd' ->
            '3'

        'e' ->
            '4'

        'f' ->
            '5'

        'g' ->
            '6'

        'h' ->
            '7'

        'i' ->
            '8'

        'j' ->
            '9'

        'k' ->
            'a'

        'l' ->
            'b'

        'm' ->
            'c'

        'n' ->
            'd'

        'o' ->
            'e'

        'p' ->
            'f'

        _ ->
            '\u{0000}'



-- | TODO: Use more efficient implementation


breakOn : String -> String -> ( String, String )
breakOn needle haystack =
    case String.split needle haystack of
        first :: rest ->
            ( first, needle ++ String.join needle rest )

        _ ->
            ( "NOT POSSIBLE", "NOT POSSIBLE" )


type alias EncodeOptions =
    { encodeLeadingDigit : Bool
    , encodeDoubleUnderscore : Bool
    }


doubleXEncodeWithOptions : EncodeOptions -> String -> String
doubleXEncodeWithOptions encodeOptions string =
    let
        encodeStandard str =
            str
                |> Debug.log "asdf"
                |> replace "XX" "XXXXXX"
                |> (\txt ->
                        if encodeOptions.encodeDoubleUnderscore then
                            txt |> replace "__" "XXRXXR"

                        else
                            txt
                   )
                |> String.toList
                |> List.map
                    (\char ->
                        if Char.isAlphaNum char || char == '_' then
                            [ char ]

                        else if charEncode char /= '\u{0000}' then
                            [ 'X', 'X', charEncode char ]

                        else
                            let
                                charHex =
                                    char
                                        |> Char.toCode
                                        |> UInt64.fromInt
                                        |> UInt64.toDigits
                                            UInt64.Digits.hexLower
                                        |> UInt64.Digits.toString

                                charHexEncoded =
                                    charHex
                                        |> String.toList
                                        |> List.map hexShiftEncode
                                        |> String.fromList
                            in
                            String.toList
                                ("XX" ++ String.padLeft 5 'a' charHexEncoded)
                    )
                |> List.concat
                |> String.fromList

        encodeDigit digit =
            String.fromList [ 'X', 'X', 'Z', digit ]
    in
    if encodeOptions.encodeLeadingDigit then
        case String.toList string of
            [] ->
                ""

            leadingChar :: rest ->
                if not <| Char.isDigit leadingChar then
                    encodeStandard string

                else if List.isEmpty rest then
                    encodeDigit leadingChar

                else
                    encodeDigit leadingChar
                        ++ doubleXEncodeWithOptions
                            { encodeOptions | encodeLeadingDigit = False }
                            (String.fromList rest)

    else
        encodeStandard string


defaultOptions : EncodeOptions
defaultOptions =
    { encodeLeadingDigit = False
    , encodeDoubleUnderscore = False
    }


doubleXEncode : String -> String
doubleXEncode =
    doubleXEncodeWithOptions defaultOptions


gqlOptions : EncodeOptions
gqlOptions =
    { encodeLeadingDigit = True
    , encodeDoubleUnderscore = True
    }


doubleXEncodeGql : String -> String
doubleXEncodeGql =
    doubleXEncodeWithOptions gqlOptions


parseHex : String -> Int
parseHex str =
    UInt64.fromString ("0x" ++ str)
        |> Maybe.andThen UInt64.toInt31
        |> Maybe.withDefault 0


doubleXDecode : String -> String
doubleXDecode text =
    let
        decodeWord : String -> ( String, String )
        decodeWord word =
            let
                noXX =
                    word |> String.dropLeft 2

                first =
                    noXX |> String.left 1
            in
            if first >= "a" && first <= "p" then
                ( String.fromList
                    [ noXX
                        |> String.toList
                        |> List.take 5
                        |> List.map hexShiftDecode
                        |> String.fromList
                        |> parseHex
                        |> Char.fromCode
                    ]
                , noXX |> String.dropLeft 5
                )

            else if first == "X" then
                if noXX |> String.startsWith "XXXX" then
                    ( "XX", noXX |> String.dropLeft 4 )

                else
                    ( "X", word |> String.dropLeft 1 )

            else if first == "Z" then
                ( noXX
                    |> String.dropLeft 1
                    |> String.left 1
                , noXX |> String.dropLeft 2
                )

            else
                ( noXX
                    |> String.left 1
                    |> String.toList
                    |> List.map charDecode
                    |> String.fromList
                , noXX
                    |> String.dropLeft 1
                )
    in
    text
        |> breakOn "XX"
        |> (\val ->
                case val of
                    ( "", end ) ->
                        case decodeWord end of
                            ( decoded, "" ) ->
                                decoded

                            ( decoded, rest ) ->
                                decoded ++ doubleXDecode rest

                    ( start, "" ) ->
                        start

                    ( start, end ) ->
                        start ++ doubleXDecode end
           )
