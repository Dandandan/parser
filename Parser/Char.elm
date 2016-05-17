module Parser.Char exposing (upper, lower, between, parenthesized, bracketed, braced, quoted, singleQuoted)

{-| Parsing characters

@docs upper, lower, between, parenthesized, bracketed, braced, quoted, singleQuoted

-}

import Char
import Parser exposing (..)

{-| Parse a upper case character -}
upper : Parser Char
upper =
    satisfy Char.isUpper

{-| Parse a lower case character -}
lower : Parser Char
lower =
    satisfy Char.isLower

{-| Parse a parser between two `Chars` -}
between : Char -> Char -> Parser result-> Parser result
between x y parser =
    symbol x *> parser <* symbol y

{-| Parse a parser between parentheses `(` and `)`-}
parenthesized : Parser result -> Parser result
parenthesized =
    between '(' ')'

{-| Parses a parser between brackets `[` and `]` -}
bracketed : Parser result -> Parser result
bracketed =
    between '[' ']'

{-| Parses a parser between braces `{` and `}`-}
braced : Parser result -> Parser result
braced =
    between '{' '}'

{-| Parses a parser between quotes `"` and `"`-}
quoted : Parser result -> Parser result
quoted =
    between '"' '"'

{-| Parses a parser between quotes `'` and `'`-}
singleQuoted : Parser result -> Parser result
singleQuoted =
    between '\'' '\''
