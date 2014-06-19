module Parser.Char where

{-| Some parsers

@docs digit, natural, integer, isSpace

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
integer = (always (\x -> -x) <$> (symbol '-')) `option` id  <*> natural

parenthesised : Parser Char a -> Parser Char a
parenthesised p = symbol '(' *> p <*symbol ')'

bracketed : Parser Char a -> Parser Char a
bracketed p = symbol '[' *> p <* symbol ']'

braced : Parser Char a -> Parser Char a
braced p = symbol '{' *>  p <* symbol '}'
