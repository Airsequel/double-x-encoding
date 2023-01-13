module Tests exposing (..)

import Blns exposing (blns)
import DoubleXEncode exposing (..)
import Expect exposing (equal)
import Fuzz exposing (string)
import Test exposing (..)


suite : Test
suite =
    describe "Double-X-Encoding"
        ((blns
            |> List.indexedMap
                (\idx str ->
                    test
                        ("Encodes and decodes naughty string "
                            ++ String.fromInt idx
                        )
                    <|
                        \() ->
                            str
                                |> DoubleXEncode.doubleXEncode
                                |> DoubleXEncode.doubleXDecode
                                |> equal str
                )
         )
            ++ [ fuzzWith
                    { runs = 10000, distribution = noDistribution }
                    string
                    "Encodes and decodes a fuzzy string"
                 <|
                    \str ->
                        str
                            |> DoubleXEncode.doubleXEncode
                            |> DoubleXEncode.doubleXDecode
                            |> equal str
               , describe "Support characters from the Unicode Plane SPUA-B"
                    [ test "Encodes and decodes codepoint U+100000" <|
                        \() ->
                            "\u{100000}"
                                |> DoubleXEncode.doubleXEncode
                                |> DoubleXEncode.doubleXDecode
                                |> equal "\u{100000}"
                    , test "Encodes and decodes codepoint U+10FFFF" <|
                        \() ->
                            "\u{10FFFF}"
                                |> DoubleXEncode.doubleXEncode
                                |> DoubleXEncode.doubleXDecode
                                |> equal "\u{10FFFF}"
                    ]
               ]
        )
