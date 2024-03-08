{-# LANGUAGE OverloadedStrings #-}

{- ORMOLU_DISABLE -}

module Main where

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor ((<&>))
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Numeric (showHex)
import Data.Char (ord, isDigit)
import Control.Monad (join)

import DoubleXEncoding
  (EncodeOptions(..), doubleXEncode, doubleXEncodeGql, doubleXDecode)


main :: IO ()
main = do
  let
    getEncoderTest encoder a b = T.putStrLn $
      if encoder a /= b
      then
        "❌ ENCODE ERROR: "
        <> a <> " | " <> encoder a <> " /= " <> b
      else
        if doubleXDecode (encoder a) /= a
        then
          "❌ DECODE ERROR: "
          <> a <> " | " <> doubleXDecode (encoder a) <> " /= " <> a
        else
          "✅ " <> a <> ": " <> encoder a <> " == " <> b


    expectEncode a b =
      getEncoderTest doubleXEncode a b

    expectEncodeGql a b =
      getEncoderTest doubleXEncodeGql a b

    expectDecode a b = T.putStrLn $
        if doubleXDecode a /= b
        then
            "❌ DECODE ERROR: "
            <> a <> " | " <> doubleXDecode a <> " /= " <> b
        else
            "✅ " <> a <> ": " <> doubleXDecode a <> " == " <> b

  expectEncode "camelCaseId" "camelCaseId"
  expectEncode "snake_case_id" "snake_case_id"
  expectEncode "__Schema" "__Schema"
  expectEncode "0test" "0test"
  expectEncode "doxxing" "doxxing"
  expectEncode "DOXXING" "DOXXXXXXING"
  expectEncode "XXX" "XXXXXXX"
  expectEncode ">XXX<" "XXKXXXXXXXXXI"
  expectEncode "id with spaces" "idXX0withXX0spaces"
  expectEncode "%" "XX5"
  expectEncode "id-with.special$chars!" "idXXDwithXXEspecialXX4charsXX1"
  expectEncode "id_with_ümläutß" "id_with_XXaaapmmlXXaaaoeutXXaaanp"
  expectEncode "Emoji: 😅" "EmojiXXGXX0XXbpgaf"
  expectEncode "Multi Byte Emoji: \x1f468\x200D\x1f9b2"
    "MultiXX0ByteXX0EmojiXXGXX0XXbpegiXXacaanXXbpjlc"
  -- Support characters from the Supplementary Private Use Area-B
  expectEncode "\x100000" "XXYbaaaaa"
  expectEncode "\x10FFFF" "XXYbapppp"

  expectEncodeGql "__Schema" "XXRXXRSchema"
  expectEncodeGql "also__middle" "alsoXXRXXRmiddle"
  expectEncodeGql "0test" "XXZ0test"
  expectDecode "alsoXXZ0middle" "also0middle"

  -- https://github.com/minimaxir/big-list-of-naughty-strings
  txtFile <- readFile "./tests/blns.txt"

  let
    blns = T.lines $ T.pack txtFile
    blnsFiltered = blns & filter
      (\line -> not (T.null line) && not (T.isPrefixOf "#" line))

    testEncodeDecode str =
      if str /= doubleXDecode (doubleXEncode str)
      then
        "❌ \""
        <> doubleXDecode (doubleXEncode str)
        <> "\" /= \""
        <> str
        <> "\""
      else "✅"

    testEncodeGqlDecode str =
      if str /= doubleXDecode (doubleXEncodeGql str)
      then
        "❌ \""
        <> doubleXDecode (doubleXEncodeGql str)
        <> "\" /= \""
        <> str
        <> "\""
      else "✅"

  T.putStrLn "\n\n============ ENCODE - DECODE ============"
  blnsFiltered <&> testEncodeDecode <&> T.putStr & sequence_

  T.putStrLn "\n\n========== ENCODE GQL - DECODE =========="
  blnsFiltered <&> testEncodeGqlDecode <&> T.putStr & sequence_

  pure ()
