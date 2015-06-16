module Parser.Number (digit, natural, integer, float, sign) where

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
    satisfy Char.isDigit
    |> map (\x -> Char.toCode x - 48)

{-| Parse a natural number -}
natural : Parser Int
natural =
    some digit
    |> map (List.foldl (\b a -> a * 10 + b) 0 )

{-| Parse a optional sign, succeeds with a -1 if it matches a minus `Char`, otherwise it returns 1 -}
sign : Parser Int
sign =
    let plus = always (-1) `map` symbol '-'
        min  = always 1 `map` symbol '+'
    in  optional (plus `or` min) 1

{-| Parse an integer with optional sign -}
integer : Parser Int
integer =
    map (*) sign `and` natural

{-| The fromOk function extracts the element out of a Ok, with a default. -}
fromOk : a -> Result e a -> a
fromOk d = withDefault d << toMaybe

{-| Parse a float with optional sign -}
float : Parser Float
float =
    let toFloatString (i,ds) =
          toString i ++ "." ++ String.concat (List.map toString ds)
        convertToFloat sig int digs =
          toFloat sig * (fromOk 0.0 << String.toFloat << toFloatString) (int,digs)
    in
        map convertToFloat sign `and` integer `and` (symbol '.' *> some digit)
