{-| Implementation of double-X-encoder and -decoder in Haskell -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Haskell.DoubleXEncode where

import Data.Function ((&))
import qualified Data.Text as T
import Data.Text (replace, Text)
import Data.Functor ((<&>))
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Numeric (showHex, readHex)
import Data.Char (ord, isDigit, chr)
import Control.Monad (join)
import Debug.Trace (traceShowId)


-- | charEncode mapping in Haskell
charEncode :: Char -> Char
charEncode = \case
  ' ' -> '0'
  '!' -> '1'
  '"' -> '2'
  '#' -> '3'
  '$' -> '4'
  '%' -> '5'
  '&' -> '6'
  '\'' -> '7'
  '(' -> '8'
  ')' -> '9'
  '*' -> 'A'
  '+' -> 'B'
  ',' -> 'C'
  '-' -> 'D'
  '.' -> 'E'
  '/' -> 'F'

  ':' -> 'G'
  ';' -> 'H'
  '<' -> 'I'
  '=' -> 'J'
  '>' -> 'K'
  '?' -> 'L'
  '@' -> 'M'

  '[' -> 'N'
  '\\' -> 'O'
  ']' -> 'P'
  '^' -> 'Q'
  '_' -> 'R'
  '`' -> 'S'

  '{' -> 'T'
  '|' -> 'U'
  '}' -> 'V'
  '~' -> 'W'

  -- TODO: Remove this parsing workaround. Should be "X": "X"
  'X' -> 'Y'

  -- '': 'Z',  -- Reserved for encoding digits

  _ -> '\0'


charDecode :: Char -> Char
charDecode = \case
  '0' -> ' '
  '1' -> '!'
  '2' -> '"'
  '3' -> '#'
  '4' -> '$'
  '5' -> '%'
  '6' -> '&'
  '7' -> '\''
  '8' -> '('
  '9' -> ')'
  'A' -> '*'
  'B' -> '+'
  'C' -> ','
  'D' -> '-'
  'E' -> '.'
  'F' -> '/'

  'G' -> ':'
  'H' -> ';'
  'I' -> '<'
  'J' -> '='
  'K' -> '>'
  'L' -> '?'
  'M' -> '@'

  'N' -> '['
  'O' -> '\\'
  'P' -> ']'
  'Q' -> '^'
  'R' -> '_'
  'S' -> '`'

  'T' -> '{'
  'U' -> '|'
  'V' -> '}'
  'W' -> '~'

  'Y' -> 'X'

  _ -> '\0'


hexShiftEncode :: Char -> Char
hexShiftEncode = \case
  '0' -> 'a'
  '1' -> 'b'
  '2' -> 'c'
  '3' -> 'd'
  '4' -> 'e'
  '5' -> 'f'
  '6' -> 'g'
  '7' -> 'h'
  '8' -> 'i'
  '9' -> 'j'
  'a' -> 'k'
  'b' -> 'l'
  'c' -> 'm'
  'd' -> 'n'
  'e' -> 'o'
  'f' -> 'p'

  _ -> '\0'


hexShiftDecode :: Char -> Char
hexShiftDecode = \case
  'a' -> '0'
  'b' -> '1'
  'c' -> '2'
  'd' -> '3'
  'e' -> '4'
  'f' -> '5'
  'g' -> '6'
  'h' -> '7'
  'i' -> '8'
  'j' -> '9'
  'k' -> 'a'
  'l' -> 'b'
  'm' -> 'c'
  'n' -> 'd'
  'o' -> 'e'
  'p' -> 'f'

  _ -> '\0'


data EncodeOptions = EncodeOptions
  { encodeLeadingDigit :: Bool
  , encodeDoubleUnderscore :: Bool
  }


doubleXEncodeWithOptions :: EncodeOptions -> Text -> Text
doubleXEncodeWithOptions encodeOptions text =
  let
    encodeStandard text = text
      & replace "XX" "XXXXXX"
      & (\txt ->
          if encodeOptions&encodeDoubleUnderscore
          then txt & replace "__" "XXRXXR"
          else txt
        )
      & T.unpack
      <&> (\char ->
            if isAsciiAlphaNum char || char == '_'
            then [char]
            else
              if charEncode char /= '\0'
              then ['X', 'X', charEncode char]
              else
                let
                  charHex = showHex (ord char) ""
                  charHexEncoded = charHex <&> hexShiftEncode
                  padStart txt = replicate (5 - length txt) 'a' ++ txt
                in
                  "XX" ++ padStart charHexEncoded
          )
      & join
      & T.pack

    encodeDigit digit =
      T.pack ['X', 'X', 'Z', digit]
  in
    if encodeOptions&encodeLeadingDigit
    then
      case T.unpack text of
        [] -> ""

        leadingChar : rest  ->
          if not $ isDigit leadingChar
          then encodeStandard text
          else
            if null rest
            then encodeDigit leadingChar
            else
              encodeDigit leadingChar
              <> doubleXEncodeWithOptions
                    encodeOptions { encodeLeadingDigit = False }
                    (T.pack rest)
    else
      encodeStandard text




defaultOptions :: EncodeOptions
defaultOptions = EncodeOptions
    { encodeLeadingDigit = False
    , encodeDoubleUnderscore = False
    }


doubleXEncode :: Text -> Text
doubleXEncode =
    doubleXEncodeWithOptions defaultOptions


gqlOptions :: EncodeOptions
gqlOptions = EncodeOptions
    { encodeLeadingDigit = True
    , encodeDoubleUnderscore = True
    }


doubleXEncodeGql :: Text -> Text
doubleXEncodeGql =
    doubleXEncodeWithOptions gqlOptions


parseHex :: Text -> Int
parseHex text =
  case readHex (T.unpack text) of
    [(int, "")] -> int
    _ -> 0


doubleXDecode :: Text -> Text
doubleXDecode text =
  let
    decodeWord :: Text -> (Text, Text)
    decodeWord word =
      let
        noXX = T.drop 2 word
        first = T.take 1 noXX
      in
        if  | first >= "a" && first <= "p" ->
                (T.pack [
                    noXX
                      & T.unpack
                      & take 5
                      <&> hexShiftDecode
                      & T.pack
                      & parseHex
                      & chr
                  ]
                , T.drop 5 noXX
                )

            | first == "X" ->
                if "XXXX" `T.isPrefixOf` noXX
                then ("XX", T.drop 4 noXX)
                else ("X", T.drop 1 word)

            | first == "Z" ->
                (noXX
                    & T.drop 1
                    & T.take 1
                , T.drop 2 noXX
                )

            | otherwise ->
              (noXX
                & T.take 1
                & T.unpack
                <&> charDecode
                & T.pack
              , T.drop 1 noXX
              )
  in
    text
      & T.breakOn "XX"
      & \case
          ("", end) -> case decodeWord end of
            (decoded, "") -> decoded
            (decoded, rest) -> decoded <> doubleXDecode rest
          (start, "") -> start
          (start, end) -> start <> doubleXDecode end
