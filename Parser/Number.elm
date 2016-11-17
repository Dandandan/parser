module Parser.Number exposing (digit, natural, integer, float, sign)

{-| Parsing numbers

@docs digit, natural, integer, float, sign

-}

import Char
import List
import Maybe exposing (withDefault)
import Parser exposing (..)
import Result exposing (toMaybe)
import String

{-| Parse a digit -}
digit : Parser Int
digit =
    let
        charToInt c = Char.toCode c - Char.toCode '0'
    in
        map charToInt (satisfy Char.isDigit)

{-| Parse a natural number -}
natural : Parser Int
natural =
    some digit
    |> map (List.foldl (\b a -> a * 10 + b) 0)

{-| Parse a optional sign, succeeds with a -1 if it matches a minus `Char`, otherwise it returns 1 -}
sign : Parser Int
sign =
    let
        plus =
            map (always -1) (symbol '-')
        min =
            map (always 1) (symbol '+')
    in
        optional (or plus min) 1

{-| Parse an integer with optional sign -}
integer : Parser Int
integer =
    map (*) sign
    |> andMap natural

{-| The fromOk function extracts the element out of a Ok, with a default. -}
fromOk : a -> Result e a -> a
fromOk default result =
    withDefault default (toMaybe result)

{-| Parse a float with optional sign -}
float : Parser Float
float =
    let
        toFloatString (i, ds) =
          toString i ++ "." ++ String.concat (List.map toString ds)
        convertToFloat sig int digs =
          toFloat sig * (fromOk 0.0 << String.toFloat << toFloatString) (int, digs)
    in
        map convertToFloat sign
        |> andMap integer
        |> andMap (symbol '.' *> some digit)
