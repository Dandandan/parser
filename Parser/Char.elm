module Parser.Char where

{-| Some parsers

@docs digit, natural, integer, parenthesized, bracketed, braced

-}

import Char
import Parser (..)

{-| Parse a digit -}
digit : Parser Char Int
digit = (\x -> Char.toCode x - 48) <$> satisfy Char.isDigit

{-| Parse a natural number -}
natural : Parser Char Int
natural = foldl (\b a -> a * 10 + b) 0 <$> some digit

{-| Parse an integer -}
integer : Parser Char Int
integer = (always (\x -> -x) <$> (symbol '-')) `optional` id  <*> natural

{-| Parse a upper case character -}
upper : Parser Char Char
upper = satisfy Char.isUpper

{-| Parse a lower case character -}
lower : Parser Char Char
lower = satisfy Char.isLower

{-| Parse a parser between parethesis `(` and `)`-}
parenthesized : Parser Char a -> Parser Char a
parenthesized p = symbol '(' *> p <*symbol ')'

{-| Parses a parser between brackets `[` and `]` -}
bracketed : Parser Char a -> Parser Char a
bracketed p = symbol '[' *> p <* symbol ']'

{-| Parses a parser between braces `{` and `}`-}
braced : Parser Char a -> Parser Char a
braced p = symbol '{' *>  p <* symbol '}'
