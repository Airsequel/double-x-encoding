{-| Implementation of double-X-encoder and -decoder in Haskell -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.DoubleXEncode where

import Data.Function ((&))
import qualified Data.Text as T
import Data.Text (replace, Text)
import Data.Functor ((<&>))
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Numeric (showHex)
import Data.Char (ord, isDigit)
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


hexDigitEncode :: Char -> Char
hexDigitEncode = \case
  '0' -> 'A'
  '1' -> 'B'
  '2' -> 'C'
  '3' -> 'D'
  '4' -> 'E'
  '5' -> 'F'
  '6' -> 'G'
  '7' -> 'H'
  '8' -> 'I'
  '9' -> 'J'

  _ -> '\0'


hexDigitDecode :: Char -> Char
hexDigitDecode = \case
  'A' -> '0'
  'B' -> '1'
  'C' -> '2'
  'D' -> '3'
  'E' -> '4'
  'F' -> '5'
  'G' -> '6'
  'H' -> '7'
  'I' -> '8'
  'J' -> '9'

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


doubleXEncode :: EncodeOptions -> Text -> Text
doubleXEncode encodeOptions text =
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
      T.pack ['X', 'X', 'Z', hexDigitEncode digit]
  in
    if encodeOptions&encodeLeadingDigit
    then
      case T.unpack text of
        "" -> ""

        [leadingDigit]  ->
            encodeDigit leadingDigit

        leadingDigit : rest  ->
          if not $ isDigit leadingDigit
          then encodeStandard text
          else
            let
            in
              encodeDigit leadingDigit
              <> doubleXEncode
                    encodeOptions { encodeLeadingDigit = False }
                    (T.pack rest)
    else
      encodeStandard text