module Test exposing (..)

import Blns exposing (blns)
import DoubleXEncode exposing (..)
import Html exposing (div, text)
import Html.Attributes exposing (style)


main : Html.Html msg
main =
    let
        getEncoderTest encoder a b =
            if encoder a /= b then
                "❌ ENCODE ERROR: "
                    ++ a
                    ++ " | "
                    ++ encoder a
                    ++ " /= "
                    ++ b

            else if doubleXDecode (encoder a) /= a then
                "❌ DECODE ERROR: "
                    ++ a
                    ++ " | "
                    ++ doubleXDecode (encoder a)
                    ++ " /= "
                    ++ a

            else
                "✅ " ++ a ++ ": " ++ encoder a ++ " == " ++ b

        expectEncode a b =
            getEncoderTest doubleXEncode a b

        expectEncodeGql a b =
            getEncoderTest doubleXEncodeGql a b

        blnsFiltered =
            blns
                |> List.filter
                    (\line ->
                        not (String.isEmpty line) && not (String.startsWith "#" line)
                    )

        testEncodeDecode str =
            if str /= doubleXDecode (doubleXEncode str) then
                "❌ \""
                    ++ doubleXDecode (doubleXEncode str)
                    ++ "\" /= \""
                    ++ str
                    ++ "\""

            else
                "✅ "

        testEncodeGqlDecode str =
            if str /= doubleXDecode (doubleXEncodeGql str) then
                "❌ \""
                    ++ doubleXDecode (doubleXEncodeGql str)
                    ++ "\" /= \""
                    ++ str
                    ++ "\""

            else
                "✅ "
    in
    [ expectEncode "camelCaseId" "camelCaseId"
    , expectEncode "snake_case_id" "snake_case_id"
    , expectEncode "__Schema" "__Schema"
    , expectEncode "0test" "0test"
    , expectEncode "doxxing" "doxxing"
    , expectEncode "DOXXING" "DOXXXXXXING"
    , expectEncode "XXX" "XXXXXXX"
    , expectEncode ">XXX<" "XXKXXXXXXXXXI"
    , expectEncode "id with spaces" "idXX0withXX0spaces"
    , expectEncode "%" "XX5"
    , expectEncode "id-with.special$chars!" "idXXDwithXXEspecialXX4charsXX1"
    , expectEncode "id_with_ümläutß" "id_with_XXaaapmmlXXaaaoeutXXaaanp"
    , expectEncode "Emoji: 😅" "EmojiXXGXX0XXbpgaf"
    , expectEncode "Multi Byte Emoji: 👨\u{200D}🦲"
        "MultiXX0ByteXX0EmojiXXGXX0XXbpegiXXacaanXXbpjlc"
    , expectEncodeGql "__Schema" "XXRXXRSchema"
    , expectEncodeGql "0test" "XXZAtest"
    , "========== ENCODE - DECODE =========="
    , blnsFiltered |> List.map testEncodeDecode |> String.join ""
    , "========== ENCODE GQL - DECODE =========="
    , blnsFiltered |> List.map testEncodeGqlDecode |> String.join ""
    ]
        |> List.map (\content -> div [] [ text content ])
        |> div [ style "padding" "2em" ]
