{- | Implementation of Double-X-Encoding encoder and decoder in Haskell

Main functions:

* `doubleXEncode`
* `doubleXEncodeGql`
* `doubleXDecode`
-}

{- ORMOLU_DISABLE -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}


module DoubleXEncoding where

import Data.Function ((&))
import qualified Data.Text as T
import Data.Text (replace, Text)
import Data.Functor ((<&>))
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Numeric (showHex, readHex)
import Data.Char (ord, isDigit, chr)
import Control.Monad (join)


-- | Encoder mapping for ASCII characters.
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
  'X' -> 'x'

  -- '': 'Z',  -- Reserved for encoding digits

  _ -> '\0'


-- | Decoder mapping for ASCII characters.
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

  -- TODO: Remove this parsing workaround. Should be "X": "X"
  'x' -> 'X'

  _ -> '\0'


-- | Map hex characters to an alternative hex alphabet
-- ranging from a to p instead of 0 to f.
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


-- | Map alternative hex alphabet back to the original hex alphabet.
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


{-|
  Options for encoding:

  [`encodeLeadingDigit`]: Encode the leading digit of the input string

  [`encodeDoubleUnderscore`]: Encode double underscores `__` as `XXRXXR`

  Especially relevant for GraphQL, as
  [the spec](https://spec.graphql.org/October2021/#sec-Names)
  does not allow leading digits or double underscores for field names.
-}
data EncodeOptions = EncodeOptions
  { encodeLeadingDigit :: Bool
  , encodeDoubleUnderscore :: Bool
  }


-- | Encode a text using the Double-X-Encoding algorithm with provided options.
doubleXEncodeWithOptions :: EncodeOptions -> Text -> Text
doubleXEncodeWithOptions encodeOptions text = do
  let
    encodeStandard text = text
      & replace "XX" "XXXXXX"
      & (\txt ->
          if encodeOptions.encodeDoubleUnderscore
          then txt & replace "__" "XXRXXR"
          else txt
        )
      & T.concatMap (\char ->
          if isAsciiAlphaNum char || char == '_'
          then T.singleton char
          else
            if charEncode char /= '\0'
            then T.pack ['X', 'X', charEncode char]
            else do
              let
                charHex = T.pack $ showHex (ord char) ""
                charHexEncoded = charHex & T.map hexShiftEncode
                padStart n txt =
                  ("a" & T.replicate (n - T.length txt)) <> txt
                charHexLength = T.length charHex

              if
                | charHexLength <= 5 ->
                    "XX" <> padStart 5 charHexEncoded
                | charHexLength == 6 ->
                    "XXY" <> padStart 6 charHexEncoded
                | otherwise ->
                    error "ERROR: Hex encoding is too long"
        )

    encodeDigit digit =
      T.pack ['X', 'X', 'Z', digit]

  if encodeOptions.encodeLeadingDigit
  then
    case T.uncons text of
      Nothing -> ""

      Just (leadingChar, rest) ->
        if not $ isDigit leadingChar
        then encodeStandard text
        else
          if T.null rest
          then encodeDigit leadingChar
          else
            encodeDigit leadingChar
            <> doubleXEncodeWithOptions
                  encodeOptions { encodeLeadingDigit = False }
                  rest
  else
    encodeStandard text


-- | Default options with no leading digit encoding
-- and no double underscore encoding.
defaultOptions :: EncodeOptions
defaultOptions = EncodeOptions
    { encodeLeadingDigit = False
    , encodeDoubleUnderscore = False
    }


-- | Encode a text using the Double-X-Encoding algorithm.
--
-- >>> doubleXEncode "id-with.special$chars!"
-- "idXXDwithXXEspecialXX4charsXX1"
doubleXEncode :: Text -> Text
doubleXEncode =
    doubleXEncodeWithOptions defaultOptions


-- | Default options for GraphQL encoding.
-- Leading digits or double underscores are not allowed for field names.
gqlOptions :: EncodeOptions
gqlOptions = EncodeOptions
    { encodeLeadingDigit = True
    , encodeDoubleUnderscore = True
    }


-- | Encode a text using the Double-X-Encoding algorithm with GraphQL options.
--
-- >>> doubleXEncodeGql "1FileFormat__"
-- "XXZ1FileFormatXXRXXR"
doubleXEncodeGql :: Text -> Text
doubleXEncodeGql =
    doubleXEncodeWithOptions gqlOptions


-- | Decode a Double-X-Encoding encoded text.
--
-- >>> doubleXDecode "idXXDwithXXEspecialXX4charsXX1"
-- "id-with.special$chars!"
doubleXDecode :: Text -> Text
doubleXDecode text = do
  let
    parseHex :: Text -> Int
    parseHex text =
      case readHex (T.unpack text) of
        [(int, "")] -> int
        _ -> 0

    decodeWord :: Text -> (Text, Text)
    decodeWord word = do
      let
        noXX = T.drop 2 word
        first = T.take 1 noXX

      if  | first >= "a" && first <= "p" ->
              (noXX
                    & T.take 5
                    & T.map hexShiftDecode
                    & parseHex
                    & chr
                    & T.singleton

              , T.drop 5 noXX
              )

          | first == "X" ->
              if "XXXX" `T.isPrefixOf` noXX
              then ("XX", T.drop 4 noXX)
              else ("X", T.drop 1 word)

          | first == "Y" ->
              (  noXX
                    & T.drop 1  -- Remove the "Y"
                    & T.take 6
                    & T.map hexShiftDecode
                    & parseHex
                    & chr
                    & T.singleton
              , T.drop 7 noXX
              )

          | first == "Z" ->
              (noXX
                  & T.drop 1
                  & T.take 1
              , T.drop 2 noXX
              )

          | otherwise ->
            (noXX
              & T.take 1
              & T.map charDecode
            , T.drop 1 noXX
            )

  text
    & T.breakOn "XX"
    & \case
        ("", end) -> case decodeWord end of
          (decoded, "") -> decoded
          (decoded, rest) -> decoded <> doubleXDecode rest
        (start, "") -> start
        (start, end) -> start <> doubleXDecode end
