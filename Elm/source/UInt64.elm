module UInt64 exposing
    ( digitsToString
    , fromInt
    , fromString
    , hexLower
    , toDigits
    , toInt31
    )

{-| Adapted from
<https://package.elm-lang.org/packages/malaire/elm-uint64/2.0.0/>
-}

import Bitwise


type UpperOrLower
    = Upper
    | Lower


type Base
    = Decimal
    | Hex UpperOrLower
    | Octal
    | Binary


{-|

  - `Int` is list length

-}
type Digits a
    = Digits ( Int, List a )


{-| 64-bit unsigned integer.

[`UInt64`](#UInt64) is represented internally as three unsigned integers:

  - `high`: 16-bit unsigned integer for bits 48 - 63
  - `mid`: 24-bit unsigned integer for bits 24 - 47
  - `low`: 24-bit unsigned integer for bits 0 - 23

-}
type UInt64
    = UInt64 ( UInt16, UInt24, UInt24 )



-- TYPE
-- CREATE
-- BASES


{-| Hexadecimal aka base 16, using lowercase characters.

  - Digits used with [`toDigits`](UInt64#toDigits): `0123456789abcdef`

-}
hexLower : Base
hexLower =
    Hex Lower



-- PADDING
-- MAPPING
-- CONVERSION - STRING


{-| Convert [`Digits`](#Digits) of `Char` to `String`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 7654321
        |> UInt64.toDigits Digits.decimal
        |> Digits.toString
        --> "7654321"

-}
digitsToString : Digits Char -> String
digitsToString (Digits ( _, digits )) =
    String.fromList digits



-- CONVERSION - LIST
-- CONSTANTS


{-| Maximum safe integer as [`UInt64`](#UInt64).

`2^53 - 1 = 9007199254740991 = 0x001FFFFFFFFFFFFF`

Equal to `Number.MAX_SAFE_INTEGER` in JavaScript.

See also [`isSafe`](#isSafe).

-}
maxSafe : UInt64
maxSafe =
    UInt64 ( maxSafeHighPart, max24, max24 )


{-| Maximum possible [`UInt64`](#UInt64) value.

`2^64 - 1 = 18446744073709551615 = 0xFFFFFFFFFFFFFFFF`

    UInt64.maxValue
        |> UInt64.toString
        --> "18446744073709551615"

-}
maxValue : UInt64
maxValue =
    UInt64 ( max16, max24, max24 )


{-| Number `0`
-}
zero : UInt64
zero =
    UInt64 ( 0, 0, 0 )



-- ARGUMENT HANDLING
-- CONVERSION - INT


{-| Convert `Int` to [`UInt64`](#UInt64).

  - `value`: `0 <= x <=`[`maxSafe`](#maxSafe)

See [argument handling](#argument-handling).

    UInt64.fromInt 123
        |> UInt64.toString
        --> "123"

-}
fromInt : Int -> UInt64
fromInt x =
    if x <= 0 then
        zero

    else if x <= maxSafeInt then
        let
            high =
                Basics.floor <| Basics.toFloat x / limit48

            midLow =
                x - limit48 * high

            mid =
                Basics.floor <| Basics.toFloat midLow / limit24

            low =
                midLow - limit24 * mid
        in
        UInt64 ( high, mid, low )

    else
        maxSafe


{-| Convert [`UInt64`](#UInt64) to 31-bit unsigned integer.

If [`UInt64`](#UInt64) is above `2^31 - 1`, return `Nothing`.

    UInt64.fromInt 0x7FFFFFFF
        |> UInt64.toInt31
        --> Just 0x7FFFFFFF

    UInt64.fromInt 0x80000000
        |> UInt64.toInt31
        --> Nothing

-}
toInt31 : UInt64 -> Maybe Int
toInt31 (UInt64 ( high, mid, low )) =
    if mid <= 0x7F && high == 0 then
        Just <| Bitwise.or (Bitwise.shiftLeftBy 24 mid) low

    else
        Nothing



-- CONVERSION - FLOAT


{-| Convert [`UInt64`](#UInt64) to `Float`.

Conversion is exact for any value from `0` to [`maxSafe`](#maxSafe),
but above [`maxSafe`](#maxSafe) value is rounded
if it can't be represented exactly as `Float`.


## Example

`9007199254740993` can't be represented exactly as `Float`,
so it's rounded to `9007199254740992`.

    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.toFloat
        --> 9007199254740992

-}
toFloat : UInt64 -> Float
toFloat (UInt64 ( high, mid, low )) =
    (Basics.toFloat high * limit24 + Basics.toFloat mid)
        * limit24
        + Basics.toFloat low



-- CONVERSION - STRING


{-| Convert `String` to [`UInt64`](#UInt64).

`String` can be

  - decimal `String` of digits `0123456789`
  - hexadecimal `String` with prefix `0x` and digits `0123456789ABCDEFabcdef`
  - octal `String` with prefix `0o` and digits `01234567`
  - binary `String` with prefix `0b` and digits `01`

Return `Nothing` if `String` isn't valid for any of the above formats,
or if the value would be above [`maxValue`](#maxValue).

See [`String` at argument handling](#-string-).

    UInt64.fromString "12345"
        |> Maybe.andThen UInt64.toInt31
        --> Just 12345

    UInt64.fromString "0x11223344AABBCCDD"
        |> Maybe.map UInt64.toInt32s
        --> Just ( 0x11223344, 0xAABBCCDD )

    UInt64.fromString "0o777"
        |> Maybe.map UInt64.toHexString
        --> Just "00000000000001FF"

    UInt64.fromString "0b1111000011110000"
        |> Maybe.map UInt64.toHexString
        --> Just "000000000000F0F0"

    -- `e` is not valid without `0x` prefix
    UInt64.fromString "1e10"
        --> Nothing

    -- value would be above `maxValue`
    UInt64.fromString "111222333444555666777"
        --> Nothing

-}
fromString : String -> Maybe UInt64
fromString str =
    let
        fromNonDecimalString charToDigit bitsPerDigit drop2 =
            case String.toList drop2 of
                [] ->
                    Nothing

                nonEmptyChars ->
                    riskyFromNonEmptyNonDecimalChars
                        charToDigit
                        bitsPerDigit
                        nonEmptyChars
    in
    case String.uncons str of
        Just ( '0', drop1 ) ->
            case String.uncons drop1 of
                Just ( 'x', drop2 ) ->
                    fromNonDecimalString charToHexDigit 4 drop2

                Just ( 'o', drop2 ) ->
                    fromNonDecimalString charToOctalDigit 3 drop2

                Just ( 'b', drop2 ) ->
                    fromNonDecimalString charToBinaryDigit 1 drop2

                _ ->
                    fromDecimalString str

        _ ->
            fromDecimalString str


{-| Convert [`UInt64`](#UInt64) to decimal `String`.

    UInt64.fromInt 0xFFFFFF
        |> UInt64.toString
        --> "16777215"

**Note:** See [Conversion - Digits](#conversion-digits) for more options
converting [`UInt64`](#UInt64) to`String`.

-}
toString : UInt64 -> String
toString x =
    -- `UInt64` is split to two parts
    -- > maxValue = 1844674407370|9551615
    -- > `divisor < 2^29` allows faster division
    -- > `lowDecimal < 2^24` fits within 24-bit `low` part
    -- > `highDecimal < 2^48` fits within 48-bit `mid/low` parts
    -- > `highDecimal < maxSafe` can be converted with `String.fromInt`
    let
        divisor =
            UInt64 ( 0, 0, 10000000 )

        ( highDecimal, lowDecimal ) =
            divMod x divisor

        highDecimalToDigits (UInt64 ( _, mid, low )) =
            String.fromInt <| low + limit24 * mid

        lowDecimalToDigits (UInt64 ( _, _, low )) =
            String.fromInt low
    in
    if isZero highDecimal then
        lowDecimalToDigits lowDecimal

    else
        highDecimalToDigits highDecimal
            ++ (String.padLeft 7 '0' <| lowDecimalToDigits lowDecimal)



-- CONVERSION - DIGITS


{-| Convert [`UInt64`](#UInt64) to [`Digits`](UInt64.Digits#Digits) of `Char`
using given [`Base`](UInt64.Digits#Base).

This is intended as first step in converting [`UInt64`](#UInt64) to `String`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.maxValue
        |> UInt64.toDigits Digits.octal
        |> Digits.toString
        --> "1777777777777777777777"

    UInt64.floor 1e15
        |> UInt64.toDigits Digits.hexLower
        |> Digits.padToMultipleOf 4 '0'
        |> Digits.groupToString 4 ' '
        --> "0003 8d7e a4c6 8000"

-}
toDigits : Base -> UInt64 -> Digits Char
toDigits base x =
    if isZero x then
        Digits ( 1, [ '0' ] )

    else
        case base of
            Decimal ->
                let
                    digitsStr =
                        toString x
                in
                Digits ( String.length digitsStr, String.toList digitsStr )

            Hex upperOrLower ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                if upperOrLower == Upper then
                    Digits
                        ( digitCount
                        , List.map riskyIntToUpperCaseDigit digitsInt
                        )

                else
                    Digits
                        ( digitCount
                        , List.map riskyIntToLowerCaseDigit digitsInt
                        )

            Octal ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                Digits
                    ( digitCount
                    , List.map riskyIntToUpperCaseDigit digitsInt
                    )

            Binary ->
                let
                    (Digits ( digitCount, digitsInt )) =
                        toIntDigits base x
                in
                Digits
                    ( digitCount
                    , List.map riskyIntToUpperCaseDigit digitsInt
                    )


{-| Convert [`UInt64`](#UInt64) to [`Digits`](UInt64.Digits#Digits) of `Int`
using given [`Base`](UInt64.Digits#Base).

This is like [`toDigits`](#toDigits)
except that each digit will be `Int` instead of `Char`.

    import UInt64
    import UInt64.Digits as Digits

    UInt64.fromInt 0xABC
        |> UInt64.toIntDigits Digits.hex
        |> Digits.toList
        --> [ 10, 11, 12 ]

    -- digit sum of 1234 is `1+2+3+4 = 10`
    UInt64.fromInt 1234
        |> UInt64.toIntDigits Digits.decimal
        |> Digits.toList
        |> List.sum
        --> 10

-}
toIntDigits : Base -> UInt64 -> Digits Int
toIntDigits base x =
    let
        toNonDecimalIntDigits bitsPerDigit partToDigits (UInt64 ( high, mid, low )) =
            if high > 0 then
                let
                    ( highCount, highDigits ) =
                        riskyIntToNonDecimalIntDigits bitsPerDigit high 0 []
                in
                Digits
                    ( highCount + 48 // bitsPerDigit
                    , highDigits ++ partToDigits mid ++ partToDigits low
                    )

            else if mid > 0 then
                let
                    ( midCount, midDigits ) =
                        riskyIntToNonDecimalIntDigits bitsPerDigit mid 0 []
                in
                Digits
                    ( midCount + 24 // bitsPerDigit
                    , midDigits ++ partToDigits low
                    )

            else
                Digits <| riskyIntToNonDecimalIntDigits bitsPerDigit low 0 []
    in
    if isZero x then
        Digits ( 1, [ 0 ] )

    else
        case base of
            Decimal ->
                toDecimalIntDigits x

            Hex _ ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 20 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 16 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 8 part
                        , Bitwise.and 0x0F <| Bitwise.shiftRightZfBy 4 part
                        , Bitwise.and 0x0F part
                        ]
                in
                toNonDecimalIntDigits 4 partToDigits x

            Octal ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 21 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 18 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 15 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 9 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 6 part
                        , Bitwise.and 0x07 <| Bitwise.shiftRightZfBy 3 part
                        , Bitwise.and 0x07 part
                        ]
                in
                toNonDecimalIntDigits 3 partToDigits x

            Binary ->
                let
                    partToDigits part =
                        [ Bitwise.shiftRightZfBy 23 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 22 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 21 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 20 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 19 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 18 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 17 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 16 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 15 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 14 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 13 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 12 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 11 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 10 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 9 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 8 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 7 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 6 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 5 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 4 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 3 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 2 part
                        , Bitwise.and 0x01 <| Bitwise.shiftRightZfBy 1 part
                        , Bitwise.and 0x01 part
                        ]
                in
                toNonDecimalIntDigits 1 partToDigits x



-- MATH


{-| Addition with wrapping overflow.

    -- `123 + 456`
    UInt64.add (UInt64.fromInt 123) (UInt64.fromInt 456)
        |> UInt64.toString
        --> "579"

    -- `maxValue + 100`
    UInt64.add UInt64.maxValue (UInt64.fromInt 100)
        |> UInt64.toString
        --> "99"

-}
add : UInt64 -> UInt64 -> UInt64
add (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA + lowB

        mid =
            if low < limit24 then
                midA + midB

            else
                midA + midB + 1

        high =
            if mid < limit24 then
                highA + highB

            else
                highA + highB + 1
    in
    UInt64
        ( Bitwise.and max16 high
        , Bitwise.and max24 mid
        , Bitwise.and max24 low
        )


{-| Decrement by one with wrapping overflow.

    -- `42 - 1`
    UInt64.decrement (UInt64.fromInt 42)
        |> UInt64.toString
        --> "41"

    -- `0 - 1`
    UInt64.decrement UInt64.zero
        |> UInt64.toHexString
        --> "FFFFFFFFFFFFFFFF"

-}
decrement : UInt64 -> UInt64
decrement (UInt64 ( high, mid, low )) =
    if low > 0 then
        UInt64 ( high, mid, low - 1 )

    else if mid > 0 then
        UInt64 ( high, mid - 1, max24 )

    else if high > 0 then
        UInt64 ( high - 1, max24, max24 )

    else
        maxValue


{-| Increment by one with wrapping overflow.

    -- `42 + 1`
    UInt64.increment (UInt64.fromInt 42)
        |> UInt64.toString
        --> "43"

    -- `maxValue + 1`
    UInt64.increment UInt64.maxValue
        |> UInt64.toString
        --> "0"

-}
increment : UInt64 -> UInt64
increment (UInt64 ( high, mid, low )) =
    if low < max24 then
        UInt64 ( high, mid, low + 1 )

    else if mid < max24 then
        UInt64 ( high, mid + 1, 0 )

    else if high < max16 then
        UInt64 ( high + 1, 0, 0 )

    else
        zero


{-| Multiplication with wrapping overflow.

    -- `1e9 * 1e9`
    UInt64.mul (UInt64.floor 1e9) (UInt64.floor 1e9)
        |> UInt64.toString
        --> "1000000000000000000"

    -- `(1e10 * 1e10) % 2^64`
    UInt64.mul (UInt64.floor 1e10) (UInt64.floor 1e10)
        |> UInt64.toString
        --> "7766279631452241920"

-}
mul : UInt64 -> UInt64 -> UInt64
mul (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        lowFull =
            lowA * lowB

        lowCarry =
            Basics.floor <| Basics.toFloat lowFull / limit24

        low =
            lowFull - lowCarry * limit24

        midFull =
            lowCarry + lowA * midB + midA * lowB

        midCarry =
            Basics.floor <| Basics.toFloat midFull / limit24

        mid =
            midFull - midCarry * limit24

        high =
            Bitwise.and max16
                (midCarry + lowA * highB + midA * midB + highA * lowB)
    in
    UInt64 ( high, mid, low )


{-| Subtraction with wrapping overflow.

    -- `456 - 123`
    UInt64.sub (UInt64.fromInt 456) (UInt64.fromInt 123)
        |> UInt64.toString
        --> "333"

    -- `0 - 0xFF`
    UInt64.sub UInt64.zero (UInt64.fromInt 0xFF)
        |> UInt64.toHexString
        --> "FFFFFFFFFFFFFF01"

-}
sub : UInt64 -> UInt64 -> UInt64
sub (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA - lowB

        mid =
            if low >= 0 then
                midA - midB

            else
                midA - midB - 1

        high =
            if mid >= 0 then
                highA - highB

            else
                highA - highB - 1
    in
    UInt64
        ( Bitwise.and max16 high
        , Bitwise.and max24 mid
        , Bitwise.and max24 low
        )



-- DIVISION


{-| Integer division with modulo.

`divMod a b` is same as `( div a b, mod a b )` but faster.

  - If divisor is [`zero`](#zero), return `( zero, zero )`.


## Examples

    UInt64.divMod (UInt64.fromInt 1234) (UInt64.fromInt 100)
        |> Tuple.mapBoth UInt64.toFloat UInt64.toFloat
        --> ( 12, 34 )

    -- ( 0xFFFFFFFFFFFFFFFF / 1e10, 0xFFFFFFFFFFFFFFFF % 1e10 )
    UInt64.divMod UInt64.maxValue (UInt64.floor 1e10)
        |> Tuple.mapBoth UInt64.toFloat UInt64.toFloat
        --> ( 1844674407, 3709551615 )

-}
divMod : UInt64 -> UInt64 -> ( UInt64, UInt64 )
divMod dividend ((UInt64 ( divisorHigh, divisorMid, divisorLow )) as divisor) =
    if divisorMid <= 0x1F && divisorHigh == 0 then
        -- divisor < 2^29
        if divisorLow == 0 && divisorMid == 0 then
            ( zero, zero )

        else
            -- `divisor` can be at most 29 bits
            -- Limiting factor is that
            -- `highMidCarry * limit24 + dividendLow` <= `2 ^ 53 - 1`.
            let
                (UInt64 ( dividendHigh, dividendMid, dividendLow )) =
                    dividend

                dividendHighMid =
                    dividendHigh * limit24 + dividendMid

                divisorFloat =
                    toFloat divisor

                quotHighMid =
                    Basics.floor <|
                        Basics.toFloat dividendHighMid
                            / divisorFloat

                highMidCarry =
                    Basics.toFloat dividendHighMid
                        - divisorFloat
                        * Basics.toFloat quotHighMid

                highMidCarryWithLow =
                    highMidCarry * limit24 + Basics.toFloat dividendLow

                quotLow =
                    Basics.floor <| highMidCarryWithLow / divisorFloat

                quotHigh =
                    Basics.floor <| Basics.toFloat quotHighMid / limit24

                quotMid =
                    quotHighMid - quotHigh * limit24

                modMidLow =
                    highMidCarryWithLow - divisorFloat * Basics.toFloat quotLow

                modMid =
                    Basics.floor <| modMidLow / limit24

                modLow =
                    Basics.floor modMidLow - modMid * limit24
            in
            ( UInt64 ( quotHigh, quotMid, quotLow )
            , UInt64 ( 0, modMid, modLow )
            )

    else if isSafe dividend then
        -- dividend < 2^53 && divisor >= 2^29
        --
        -- !!! divisor can be outside safe range !!!
        -- --> quotInt  = 0
        -- --> modFloat = dividendFloat
        -- --> algorithm works correctly and returns ( zero, dividend )
        let
            dividendFloat =
                toFloat dividend

            divisorFloat =
                toFloat divisor

            quotInt =
                Basics.floor <| dividendFloat / divisorFloat

            quotHigh =
                Basics.floor <| Basics.toFloat quotInt / limit48

            quotMidLow =
                quotInt - limit48 * quotHigh

            quotMid =
                Basics.floor <| Basics.toFloat quotMidLow / limit24

            quotLow =
                quotMidLow - limit24 * quotMid

            modFloat =
                dividendFloat - divisorFloat * Basics.toFloat quotInt

            modHigh =
                Basics.floor <| modFloat / limit48

            modMidLow =
                modFloat - limit48 * Basics.toFloat modHigh

            modMid =
                Basics.floor <| modMidLow / limit24

            modLow =
                modMidLow - limit24 * Basics.toFloat modMid
        in
        ( UInt64 ( quotHigh, quotMid, quotLow )
        , UInt64 ( modHigh, modMid, Basics.floor modLow )
        )

    else
        -- dividend >= 2^53 && divisor >= 2^29
        case divModFast dividend divisor of
            Ok divMod_ ->
                divMod_

            Err _ ->
                -- IMPOSSIBLE: I believe this case is impossible to reach
                -- But in case the impossible happens,
                -- use the slow and simple algorithm.
                divModSlow dividend divisor



-- DIVISION - EXTRA


{-| Fast but complex algorithm for integer division with modulo.

  - If divisor is [`zero`](#zero), return `Ok ( zero, zero )`.
  - If impossible happens, return `Err error`.
      - This should never happen, but see [`divModSlow`](#divModSlow).

You should usually use [`divMod`](#divMod) instead because it will use
even faster algorithms when `dividend < 2^53` or `divisor < 2^29`,
and will fall back to [`divModFast`](#divModFast) otherwise.

So [`divModFast`](#divModFast) is faster than [`divMod`](#divMod) only when
`dividend >= 2^53 && divisor >= 2^29`.

-}
divModFast : UInt64 -> UInt64 -> Result String ( UInt64, UInt64 )
divModFast dividend divisor =
    -- !!! This is custom algorithm made up by myself from scratch !!!
    -- I'm surprised it actually seems to be working ... :D
    --
    -- Basic idea:
    --   STEP 1) calculate approximate division using floating point division
    --   STEP 2) analyze error of approximation and handle it
    --
    if isZero divisor then
        -- be consistent with other division algorithms
        Ok ( zero, zero )

    else
        let
            -- approximate divisor (exact if `isSafe divisor`)
            approxDivisor =
                toFloat divisor

            -- approximate division (exact if `isSafe dividend && isSafe divisor`)
            approxDiv =
                riskyFloatTo64 <|
                    floorAnyPositiveFloat <|
                        toFloat dividend
                            / approxDivisor

            -- only 65 bits are actually needed,
            -- but implementing UInt72 is exactly as simple and fast as UInt65
            approxDivMultiplied72 : UInt72
            approxDivMultiplied72 =
                mul64_64_72 approxDiv divisor
        in
        -- compare `approxDiv` to `dividend / divisor`
        -- <==>   compare `approxDiv * divisor` to `dividend`
        ------------------==============================
        case compare72_64 approxDivMultiplied72 dividend of
            --------------==============================
            EQ ->
                -- *** approxDivMultiplied72 == dividend ***
                -- approxDiv is exactly correct, which means modulo is zero
                Ok ( approxDiv, zero )

            LT ->
                -- *** approxDivMultiplied72 < dividend ***
                -- approxDiv is smaller than `dividend / divisor`. Then check the delta ...
                let
                    -- `dividend - approxDiv * divisor`, this could be the modulo ...
                    delta =
                        sub dividend (risky72to64 approxDivMultiplied72)
                in
                -- is `delta` small enough to be modulo ?
                -------------=============
                case compare delta divisor of
                    ---------=============
                    LT ->
                        -- *** delta < divisor ***
                        -- delta is small enough to be modulo, so we have the answer
                        Ok ( approxDiv, delta )

                    EQ ->
                        -- *** delta == divisor ***
                        -- modulo can't equal divisor,
                        -- but this means `approxDiv` is just 1 too small and real modulo is zero
                        Ok ( increment approxDiv, zero )

                    GT ->
                        -- *** delta > divisor ***
                        -- delta is too large to be modulo, so `approxDiv` is far too small
                        let
                            -- add `delta / approxDivisor` to `approxDiv` to create new approximation
                            secondApproxDiv =
                                add
                                    approxDiv
                                    (riskyFloatTo64 <|
                                        floorAnyPositiveFloat <|
                                            toFloat delta
                                                / approxDivisor
                                    )

                            secondApproxDivMultiplied =
                                mul secondApproxDiv divisor
                        in
                        -- compare `secondApproxDiv` to `dividend / divisor`
                        -------------==================================
                        case compare secondApproxDivMultiplied dividend of
                            ---------==================================
                            EQ ->
                                -- *** secondApproxDivMultiplied == dividend ***
                                -- secondApproxDiv is exactly correct, which means modulo is zero
                                Ok ( secondApproxDiv, zero )

                            LT ->
                                -- *** secondApproxDivMultiplied < dividend ***
                                -- secondApproxDiv is smaller than `dividend / divisor`. Then check the delta ...
                                let
                                    -- `dividend - approxDiv * divisor`, this could be the modulo ...
                                    secondDelta =
                                        sub dividend secondApproxDivMultiplied
                                in
                                -- is `secondDelta` small enough to be modulo ?
                                -------------===================
                                case compare secondDelta divisor of
                                    ---------===================
                                    LT ->
                                        -- *** secondDelta < divisor ***
                                        -- secondDelta is small enough to be modulo, so we have the answer
                                        Ok ( secondApproxDiv, secondDelta )

                                    _ ->
                                        -- *** secondDelta >= divisor ***
                                        -- IMPOSSIBLE: I believe this case is impossible to reach
                                        Err "IMPOSSIBLE: secondDelta >= divisor"

                            GT ->
                                -- *** secondApproxDivMultiplied > dividend ***
                                -- IMPOSSIBLE: I believe this case is impossible to reach
                                Err "IMPOSSIBLE: secondApproxDivMultiplied > dividend"

            GT ->
                -- *** approxDivMultiplied72 > dividend ***
                -- approxDiv is larger than `dividend / divisor`
                -- This means that approxDiv will need fixing, no matter how large delta72 is. Then check the delta72 ...
                let
                    delta72 : UInt72
                    delta72 =
                        sub72_64_72 approxDivMultiplied72 dividend
                in
                -- is `delta72` small enough to be modulo ?
                ------------------===============
                case compare72_64 delta72 divisor of
                    --------------===============
                    LT ->
                        -- *** delta72 < divisor ***
                        -- delta72 is small enough to be modulo. This means `approxDiv` is just 1 too large,
                        --   so fix `approxDiv` and calculate correct modulo
                        Ok ( decrement approxDiv, sub divisor (risky72to64 delta72) )

                    EQ ->
                        -- *** delta72 == divisor ***
                        -- modulo can't equal divisor,
                        -- but this means `approxDiv` is just 1 too large and real modulo is zero
                        -- NOTE: I've only seen this case being used with small divisors.
                        --       Largest divisor seen so far is 1923 with `UInt64.maxValue / 1923`.
                        Ok ( decrement approxDiv, zero )

                    GT ->
                        -- *** delta72 > divisor ***
                        -- delta72 is too large to be modulo, so `approxDiv` is far too large
                        -- NOTE: I've only seen this case being used with small divisors.
                        --       Largest divisor seen so far is 972 with `UInt64.maxValue / 972`.
                        -- ==> I'll limit this logic to `isSafe` values,
                        --     so exact integer-Float math can be used for speed
                        if isSafe72 delta72 then
                            let
                                -- split `delta72` to div/mod pair
                                -- Since here `divisor < delta72 <= maxSafeAsFloat`,
                                -- it follows that `approxDivisor` is exact
                                ( deltaDiv, deltaMod ) =
                                    divModFloat (toFloat72 delta72) approxDivisor
                            in
                            if deltaMod == 0 then
                                -- modulo is zero, so only `approxDiv` needs fixing
                                Ok ( sub approxDiv (riskyFloatTo64 deltaDiv), zero )

                            else
                                -- modulo is non-zero, so both `approxDiv` and modulo needs fixing
                                Ok
                                    ( sub approxDiv (riskyFloatTo64 <| deltaDiv + 1)
                                    , riskyFloatTo64 <| approxDivisor - deltaMod
                                    )

                        else
                            -- IMPOSSIBLE: I believe this case is impossible to reach
                            Err <|
                                "IMPOSSIBLE: approxDivMultiplied72 > dividend "
                                    ++ "&& not (isSafe72 delta72)"


{-| Simple but slow [long division][LD]🢅 algorithm for integer division with modulo.

  - If divisor is [`zero`](#zero), return `( zero, zero )`.

Intended use cases:

  - Benchmarking against other algorithms.
  - Used as fallback algorithm with [`divModFast`](#divModFast),
    in case it returns `Err`. Which should never happen.

[LD]: https://en.wikipedia.org/wiki/Division_algorithm#Long_division

-}
divModSlow : UInt64 -> UInt64 -> ( UInt64, UInt64 )
divModSlow dividend divisor =
    if isZero divisor then
        ( zero, zero )

    else
        List.foldl
            (\n ( div_, mod_ ) ->
                let
                    newMod =
                        setBit 0 (getBit n dividend) <| shiftLeftBy 1 mod_
                in
                if compare newMod divisor /= LT then
                    ( setBit n 1 div_, sub newMod divisor )

                else
                    ( div_, newMod )
            )
            ( zero, zero )
            -- I could pre-create this list, but this algorithm is meant to be
            -- the simple 100% correct one with no possibility of error.
            (List.reverse <| List.range 0 63)



-- BITWISE


{-| Return a bit.

  - `bitNumber`: `0 <= x <= 63`, least significant bit is `0`

See [argument handling](#argument-handling).

    UInt64.one
        |> UInt64.getBit 0
        --> 1

-}
getBit : Int -> UInt64 -> Int
getBit givenBitNumber (UInt64 ( high, mid, low )) =
    let
        bitNumber =
            Bitwise.and 0x3F givenBitNumber
    in
    if bitNumber < 24 then
        Bitwise.and 1 <| Bitwise.shiftRightZfBy bitNumber low

    else if bitNumber < 48 then
        Bitwise.and 1 <| Bitwise.shiftRightZfBy (bitNumber - 24) mid

    else
        -- bitNumber < 64
        Bitwise.and 1 <| Bitwise.shiftRightZfBy (bitNumber - 48) high


{-| Set a bit to given value.

  - `bitNumber`: `0 <= x <= 63`, least significant bit is `0`
  - `bitValue`: new value, either `0` or `1`

See [argument handling](#argument-handling).

    UInt64.zero
        |> UInt64.setBit 30 1
        |> UInt64.toHexString
        --> "0000000040000000"

-}
setBit : Int -> Int -> UInt64 -> UInt64
setBit givenBitNumber givenBitValue (UInt64 ( high, mid, low )) =
    let
        bitNumber =
            Bitwise.and 0x3F givenBitNumber

        bitValue =
            Bitwise.and 0x01 givenBitValue
    in
    if bitNumber < 24 then
        if bitValue == 0 then
            UInt64
                ( high
                , mid
                , Bitwise.and low <|
                    Bitwise.complement <|
                        Bitwise.shiftLeftBy bitNumber 1
                )

        else
            UInt64
                ( high
                , mid
                , Bitwise.or low <|
                    Bitwise.shiftLeftBy bitNumber 1
                )

    else if bitNumber < 48 then
        if bitValue == 0 then
            UInt64
                ( high
                , Bitwise.and mid <|
                    Bitwise.complement <|
                        Bitwise.shiftLeftBy (bitNumber - 24) 1
                , low
                )

        else
            UInt64
                ( high
                , Bitwise.or mid <|
                    Bitwise.shiftLeftBy (bitNumber - 24) 1
                , low
                )

    else if bitValue == 0 then
        -- bitNumber < 64
        UInt64
            ( Bitwise.and high <|
                Bitwise.complement <|
                    Bitwise.shiftLeftBy (bitNumber - 48) 1
            , mid
            , low
            )

    else
        -- bitNumber < 64
        UInt64
            ( Bitwise.or high <|
                Bitwise.shiftLeftBy (bitNumber - 48) 1
            , mid
            , low
            )


{-| Bitwise shift left, filling with zeroes from right.

  - `shift`: `0 <= x <= 63`

See [argument handling](#argument-handling).

    UInt64.fromInt32s 0x11223344 0xAABBCCDD
        |> UInt64.shiftLeftBy 20
        |> UInt64.toHexString
        --> "344AABBCCDD00000"

-}
shiftLeftBy : Int -> UInt64 -> UInt64
shiftLeftBy givenShift (UInt64 ( high, mid, low )) =
    let
        n =
            Bitwise.and 0x3F givenShift
    in
    --  0: HHHH MMMMMM LLLLLL
    -- 24: MMMM LLLLLL 000000
    -- 48: LLLL 000000 000000
    if n < 24 then
        UInt64
            ( Bitwise.and max16 <|
                Bitwise.or
                    (Bitwise.shiftLeftBy n high)
                    (Bitwise.shiftRightZfBy (24 - n) mid)
            , Bitwise.and max24 <|
                Bitwise.or
                    (Bitwise.shiftLeftBy n mid)
                    (Bitwise.shiftRightZfBy (24 - n) low)
            , Bitwise.and max24 <|
                Bitwise.shiftLeftBy
                    n
                    low
            )

    else if n < 48 then
        UInt64
            ( Bitwise.and max16 <|
                Bitwise.or
                    (Bitwise.shiftLeftBy (n - 24) mid)
                    (Bitwise.shiftRightZfBy (48 - n) low)
            , Bitwise.and max24 <|
                Bitwise.shiftLeftBy
                    (n - 24)
                    low
            , 0
            )

    else
        -- n < 64
        UInt64
            ( Bitwise.and max16 <|
                Bitwise.shiftLeftBy
                    (n - 48)
                    low
            , 0
            , 0
            )



-- COMPARISON


{-| Compare two [`UInt64`](#UInt64):s.

    UInt64.compare UInt64.zero UInt64.one
        --> LT

-}
compare : UInt64 -> UInt64 -> Basics.Order
compare (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    case Basics.compare highA highB of
        EQ ->
            case Basics.compare midA midB of
                EQ ->
                    Basics.compare lowA lowB

                midNotEq ->
                    midNotEq

        highNotEq ->
            highNotEq



-- CHECK


{-| Return `True` if argument is safe integer.

A safe integer is an integer that

  - can be represented exacly as `Float` and
  - no other integer is rounded to as `Float`.

Unsigned integers from `0` to [`maxSafe`](#maxSafe) are safe integers.


## Example

For example `2^53` is not a safe integer.
While it can be represented exactly as `Float`,
there exists another integer `2^53 + 1` which is rounded to `2^53`:

    -- 2^53 + 1 (9007199254740993) is rounded to
    -- 2^53     (9007199254740992) when converted to Float
    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.toFloat
        --> 9007199254740992

    UInt64.fromDecimal12s 9007 199254740993
        |> UInt64.isSafe
        --> False

This happens because `2^53 + 1` can't be represented exactly as `Float`,
so it is rounded to another integer.

-}
isSafe : UInt64 -> Bool
isSafe (UInt64 ( high, _, _ )) =
    high <= maxSafeHighPart


{-| Return `True` if argument is [`zero`](#zero).

This is same as `(==)`[`zero`](#zero) but much faster.

**Note:**
See [Performance Optimization][PO]🢅
for discussion about speed of `==` in Elm 0.19.1.

[PO]: https://discourse.elm-lang.org/t/performance-optimization/5105

-}
isZero : UInt64 -> Bool
isZero (UInt64 ( high, mid, low )) =
    low == 0 && mid == 0 && high == 0



-- INTERNAL - TYPES


type alias UInt16 =
    Int


type alias UInt24 =
    Int


{-| Internal type that uses 24 bits also for high-part.

So far I've only needed up to 65 bits,
but implementing 65 or 72 is equally fast,
and 72 is actually a bit simpler.

-}
type UInt72
    = UInt72 ( UInt24, UInt24, UInt24 )



-- INTERNAL - CONSTANTS


limit24 : number
limit24 =
    0x01000000


limit48 : number
limit48 =
    0x0001000000000000


max16 : number
max16 =
    0xFFFF


max24 : number
max24 =
    0x00FFFFFF


{-| Minimum value known for certain to be an integer,
and so doesn't need to use `Basics.floor`.

`2 ^ 52 = 4503599627370496`

-}
minCertainInteger : Float
minCertainInteger =
    4503599627370496.0


maxSafeInt : Int
maxSafeInt =
    9007199254740991


{-| Maximum value of `high` part of UInt64/UInt72 within safe integer range
-}
maxSafeHighPart : Int
maxSafeHighPart =
    0x1F



-- INTERNAL - FLOAT


{-| Copied from SafeInt.Unchecked.divMod
-}
divModFloat : Float -> Float -> ( Float, Float )
divModFloat a b =
    let
        div_ =
            Basics.toFloat <| Basics.floor <| a / b
    in
    ( div_, a - b * div_ )


floorAnyPositiveFloat : Float -> Float
floorAnyPositiveFloat x =
    if x >= minCertainInteger then
        x

    else
        Basics.toFloat <| Basics.floor x


{-| Float must be unsigned integer within safe range.
-}
riskyFloatTo64 : Float -> UInt64
riskyFloatTo64 x =
    let
        highMid =
            Basics.floor <| x / limit24

        low =
            Basics.floor x - highMid * limit24

        high =
            Basics.floor <| Basics.toFloat highMid / limit24

        mid =
            highMid - high * limit24
    in
    UInt64 ( high, mid, low )



-- INTERNAL - CHAR / DIGIT


{-| Convert chars to digits with given function.

  - Return `Nothing` if `charToDigit` returns `Nothing` for any char of `chars`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
charListToDigits :
    (Char -> Maybe Int)
    -> Int
    -> List Int
    -> List Char
    -> Maybe ( Int, List Int )
charListToDigits charToDigit digitCount digits chars =
    case chars of
        x :: xs ->
            case charToDigit x of
                Just digit ->
                    charListToDigits charToDigit
                        (digitCount + 1)
                        (digit :: digits)
                        xs

                Nothing ->
                    Nothing

        [] ->
            Just ( digitCount, List.reverse digits )


charToBinaryDigit : Char -> Maybe Int
charToBinaryDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        _ ->
            Nothing


charToHexDigit : Char -> Maybe Int
charToHexDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


charToOctalDigit : Char -> Maybe Int
charToOctalDigit char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        _ ->
            Nothing


fromDecimalString : String -> Maybe UInt64
fromDecimalString str =
    let
        length =
            String.length str
    in
    if String.startsWith "-" str || String.startsWith "+" str then
        Nothing

    else if length <= 15 then
        case String.toInt str of
            Just int ->
                Just <| fromInt int

            Nothing ->
                Nothing

    else if length <= 20 then
        let
            lowStr =
                String.right 10 str
        in
        if String.startsWith "-" lowStr || String.startsWith "+" lowStr then
            Nothing

        else
            case ( String.toInt <| String.dropRight 10 str, String.toInt lowStr ) of
                ( Just highDecimal, Just lowDecimal ) ->
                    -- maxValue = 1844674407|3709551615
                    if
                        highDecimal
                            > 1844674407
                            || (highDecimal == 1844674407 && lowDecimal > 3709551615)
                    then
                        Nothing

                    else
                        -- highDecimal * 1e10 + lowDecimal
                        mul (fromInt highDecimal) (UInt64 ( 0, 0x0254, 0x000BE400 ))
                            |> add (fromInt lowDecimal)
                            |> Just

                _ ->
                    Nothing

    else
        -- length > 20
        let
            prefix =
                String.dropRight 20 str
        in
        if String.toInt prefix /= Just 0 then
            Nothing

        else
            fromDecimalString <| String.right 20 str


{-| Convert list of digits to `UInt64`.

  - `bitsPerDigit` must be a factor of `24`.
  - `digitCount` must equal `List.length digits`.
  - Each digit must be `0 <= digit <= 2 ^ bitsPerDigit - 1`.
  - Return `Just zero` for empty list.
  - Return `Nothing` on overflow.

-}
riskyFromNonDecimalDigits : Int -> Int -> List Int -> Maybe UInt64
riskyFromNonDecimalDigits bitsPerDigit digitCount digits =
    let
        bitCount =
            digitCount * bitsPerDigit
    in
    if bitCount <= 66 then
        -- 22 octal digits is 66 bits
        riskyFromNonDecimalDigitsHelper bitsPerDigit bitCount 0 0 0 digits

    else
        Nothing


{-| Convert hex/octal/binary digits to UInt64.

  - `bitsPerDigit` must be factor of `24`.
  - `bitCount` must equal `bitsPerDigit * List.length digits` and be within `0 <= bitCount <= 66`.
  - Each digit must be within `0 <= digit <= 2 ^ bitsPerDigit - 1`.
  - Initial call is with `high = mid = low = 0`.
  - Return `Nothing` on overflow.

p.s. This would also handle base-4 and base-64.

-}
riskyFromNonDecimalDigitsHelper : Int -> Int -> Int -> Int -> Int -> List Int -> Maybe UInt64
riskyFromNonDecimalDigitsHelper bitsPerDigit bitCount high mid low digits =
    case digits of
        x :: xs ->
            if bitCount <= 24 then
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    high
                    mid
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit low)
                    xs

            else if bitCount <= 48 then
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    high
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit mid)
                    low
                    xs

            else
                riskyFromNonDecimalDigitsHelper
                    bitsPerDigit
                    (bitCount - bitsPerDigit)
                    (Bitwise.or x <| Bitwise.shiftLeftBy bitsPerDigit high)
                    mid
                    low
                    xs

        [] ->
            if high > max16 then
                Nothing

            else
                Just <| UInt64 ( high, mid, low )


{-| Convert hex/octal/binary chars to digits and then to `UInt64`, ignoring leading zeroes.

  - `bitsPerDigit` must be a factor of `24`.
  - Return `Just zero` if `chars` is empty or has only zeroes.
  - Return `Nothing` if `charListToDigits` or `riskyFromNonDecimalDigits` returns `Nothing`.

-}
riskyFromNonEmptyNonDecimalChars : (Char -> Maybe Int) -> Int -> List Char -> Maybe UInt64
riskyFromNonEmptyNonDecimalChars charToDigit bitsPerDigit chars =
    case chars of
        '0' :: xs ->
            riskyFromNonEmptyNonDecimalChars charToDigit bitsPerDigit xs

        noLeadingZeroes ->
            case charListToDigits charToDigit 0 [] noLeadingZeroes of
                Just ( digitCount, digits ) ->
                    riskyFromNonDecimalDigits bitsPerDigit digitCount digits

                Nothing ->
                    Nothing


{-| Convert `Int` to decimal digits of `0 <= digit <= 9`.

  - Given `Int` must be `0 <= x < 2^31`.
  - Return `(0, [])` if `x == 0`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
riskyIntToDecimalIntDigits : Int -> Int -> List Int -> ( Int, List Int )
riskyIntToDecimalIntDigits x digitCount digits =
    if x == 0 then
        ( digitCount, digits )

    else
        riskyIntToDecimalIntDigits (x // 10) (digitCount + 1) (modBy 10 x :: digits)


{-| Convert `Int` to lowercase `Char` digit. Return `x` for values over 15.

Works for binary/octal/decimal/hex as long as argument is valid.

-}
riskyIntToLowerCaseDigit : Int -> Char
riskyIntToLowerCaseDigit x =
    case x of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            'x'


{-| Convert `Int` to hex/octal/binary digits of `0 <= digit <= 2 ^ bitsPerDigit - 1`.

  - `bitsPerDigit` must be a factor of `24`.
  - Given `Int` must be `0 <= x < 2^31`.
  - Return `(0, [])` if `x == 0`.
  - Return also `Length.list digits` so it doesn't need to be calculated separately.
  - Initial call is with `digitCount = 0` and `digits = []`.

-}
riskyIntToNonDecimalIntDigits : Int -> Int -> Int -> List Int -> ( Int, List Int )
riskyIntToNonDecimalIntDigits bitsPerDigit x digitCount digits =
    let
        bitMask =
            Bitwise.shiftLeftBy bitsPerDigit 1 - 1
    in
    if x == 0 then
        ( digitCount, digits )

    else
        riskyIntToNonDecimalIntDigits
            bitsPerDigit
            (Bitwise.shiftRightZfBy bitsPerDigit x)
            (digitCount + 1)
            (Bitwise.and bitMask x :: digits)


{-| Convert `Int` to uppercase `Char` digit. Return `X` for values over 15.

Works for binary/octal/decimal/hex as long as argument is valid.

-}
riskyIntToUpperCaseDigit : Int -> Char
riskyIntToUpperCaseDigit x =
    case x of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        _ ->
            'X'


toDecimalIntDigits : UInt64 -> Digits Int
toDecimalIntDigits x =
    -- `UInt64` is split to three 7-decimal-digit parts
    -- > maxValue = 184467|4407370|9551615
    -- > `divisor < 2^29` allows faster division
    -- > `each part < 2^31` allows `Int` math
    -- > `lowDecimal < 2^24` fits within 24-bit `low` part
    -- > `highMidDecimal < 2^48` fits within 48-bit `mid/low` parts
    let
        divisorInt =
            10000000

        divisor =
            UInt64 ( 0, 0, divisorInt )

        ( highMidDecimal, UInt64 ( _, _, lowDecimal ) ) =
            divMod x divisor
    in
    if isZero highMidDecimal then
        Digits <| riskyIntToDecimalIntDigits lowDecimal 0 []

    else
        let
            (UInt64 ( _, highMidDecimal_mid, highMidDecimal_low )) =
                highMidDecimal

            highMidDecimalInt =
                highMidDecimal_low + limit24 * highMidDecimal_mid

            highDecimal =
                Basics.floor <| Basics.toFloat highMidDecimalInt / divisorInt

            midDecimal =
                highMidDecimalInt - divisorInt * highDecimal
        in
        if highDecimal == 0 then
            let
                ( lowCount, lowDigits ) =
                    riskyIntToDecimalIntDigits lowDecimal 0 []

                ( midCount, midDigits ) =
                    riskyIntToDecimalIntDigits midDecimal 0 []
            in
            Digits ( midCount + 7, midDigits ++ List.repeat (7 - lowCount) 0 ++ lowDigits )

        else
            let
                ( lowCount, lowDigits ) =
                    riskyIntToDecimalIntDigits lowDecimal 0 []

                ( midCount, midDigits ) =
                    riskyIntToDecimalIntDigits midDecimal 0 []

                ( highCount, highDigits ) =
                    riskyIntToDecimalIntDigits highDecimal 0 []
            in
            Digits
                ( highCount + 14
                , highDigits
                    ++ List.repeat (7 - midCount) 0
                    ++ midDigits
                    ++ List.repeat (7 - lowCount) 0
                    ++ lowDigits
                )



-- INTERNAL - UInt72


compare72_64 : UInt72 -> UInt64 -> Basics.Order
compare72_64 (UInt72 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    case Basics.compare highA highB of
        EQ ->
            case Basics.compare midA midB of
                EQ ->
                    Basics.compare lowA lowB

                midNotEq ->
                    midNotEq

        highNotEq ->
            highNotEq


isSafe72 : UInt72 -> Bool
isSafe72 (UInt72 ( high, _, _ )) =
    high <= maxSafeHighPart


mul64_64_72 : UInt64 -> UInt64 -> UInt72
mul64_64_72 (UInt64 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        lowFull =
            lowA * lowB

        lowCarry =
            Basics.floor <| Basics.toFloat lowFull / limit24

        low =
            lowFull - lowCarry * limit24

        midFull =
            lowCarry + lowA * midB + midA * lowB

        midCarry =
            Basics.floor <| Basics.toFloat midFull / limit24

        mid =
            midFull - midCarry * limit24

        high =
            Bitwise.and max24 (midCarry + lowA * highB + midA * midB + highA * lowB)
    in
    UInt72 ( high, mid, low )


{-| Use only when UInt72 is known to be < 2^64
-}
risky72to64 : UInt72 -> UInt64
risky72to64 (UInt72 tuple) =
    UInt64 tuple


sub72_64_72 : UInt72 -> UInt64 -> UInt72
sub72_64_72 (UInt72 ( highA, midA, lowA )) (UInt64 ( highB, midB, lowB )) =
    let
        low =
            lowA - lowB

        mid =
            if low >= 0 then
                midA - midB

            else
                midA - midB - 1

        high =
            if mid >= 0 then
                highA - highB

            else
                highA - highB - 1
    in
    UInt72 ( Bitwise.and max24 high, Bitwise.and max24 mid, Bitwise.and max24 low )


toFloat72 : UInt72 -> Float
toFloat72 (UInt72 ( high, mid, low )) =
    (Basics.toFloat high * limit24 + Basics.toFloat mid) * limit24 + Basics.toFloat low
