module Parser.Char (digit, natural, integer, float, upper, lower, between, parenthesized, bracketed, braced) where

{-| Some parsers

@docs digit, natural, integer, float, upper, lower, between, parenthesized, bracketed, braced

-}

import Char
import Parser
import Parser (..)
import List
import List (..)

{-| Parse a digit -}
digit : Parser Char Int
digit = (\x -> Char.toCode x - 48) `Parser.map` satisfy Char.isDigit

{-| Parse a natural number -}
natural : Parser Char Int
natural = foldl (\b a -> a * 10 + b) 0 `Parser.map` some digit

--parse a sign
sign : Parser Char Int
sign = optional (always (-1) `Parser.map` symbol '-') 1 `or` optional (always 1 `Parser.map` symbol '+') 1

{-| Parse an integer with optional sign -}
integer : Parser Char Int
integer = (\sig nat -> sig * nat) `Parser.map` sign `and` natural

{-| Parse a float with optional sign -}
float : Parser Char Float
float =
    let convertToFloat sig int dig = toFloat sig * (toFloat int + 0.1 * foldr (\b a -> a / 10 + b) 0 (List.map toFloat dig))
    in
        convertToFloat `Parser.map` sign `and` integer `and` (symbol '.' *> some digit)

{-| Parse a upper case character -}
upper : Parser Char Char
upper = satisfy Char.isUpper

{-| Parse a lower case character -}
lower : Parser Char Char
lower = satisfy Char.isLower

{-| Parse a parser between two `Chars` -}
between : Char -> Char -> Parser Char result-> Parser Char result
between x y parser = symbol x *> parser <* symbol y

{-| Parse a parser between parentheses `(` and `)`-}
parenthesized : Parser Char result -> Parser Char result
parenthesized = between '(' ')'

{-| Parses a parser between brackets `[` and `]` -}
bracketed : Parser Char result -> Parser Char result
bracketed = between '[' ']'

{-| Parses a parser between braces `{` and `}`-}
braced : Parser Char result -> Parser Char result
braced = between '{' '}'
