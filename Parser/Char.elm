module Parser.Char (digit, natural, integer, float, upper, lower, parenthesized, bracketed, braced) where

{-| Some parsers

@docs digit, natural, integer, float, upper, lower, parenthesized, bracketed, braced

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

{-| Parse an integer -}
integer : Parser Char Int
integer = (always (\x -> -x) `Parser.map` (symbol '-')) `optional` identity `and` natural

{-| Parse a float -}
float : Parser Char Float
float = (\i f -> toFloat i + 0.1 * foldr (\b a -> a / 10 + b) 0 (List.map toFloat f))  `Parser.map` integer <* symbol '.' `and` some digit

{-| Parse a upper case character -}
upper : Parser Char Char
upper = satisfy Char.isUpper

{-| Parse a lower case character -}
lower : Parser Char Char
lower = satisfy Char.isLower

{-| Parse a parser between parentheses `(` and `)`-}
parenthesized : Parser Char r -> Parser Char r
parenthesized p = symbol '(' *> p <*symbol ')'

{-| Parses a parser between brackets `[` and `]` -}
bracketed : Parser Char r -> Parser Char r
bracketed p = symbol '[' *> p <* symbol ']'

{-| Parses a parser between braces `{` and `}`-}
braced : Parser Char r -> Parser Char r
braced p = symbol '{' *>  p <* symbol '}'
