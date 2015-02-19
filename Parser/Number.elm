module Parser.Number (digit, natural, integer, float, sign) where

{-| Parsing numbers

@docs digit, natural, integer, float, sign

-}

import Char
import Parser (..)
import List

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
    map (\sig nat -> sig * nat) sign `and` natural

{-| Parse a float with optional sign -}
float : Parser Float
float =
    let convertToFloat sig int dig = toFloat sig * (toFloat int + 0.1 * List.foldr (\b a -> a / 10 + b) 0 (List.map toFloat dig))
    in
        map convertToFloat sign `and` integer `and` (symbol '.' *> some digit)
