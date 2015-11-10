**Parser**
======

Parsers convert text sequences into a data structure, for example
for handling user input and compiling programming languages.

With this parser combinator you can build parsers in a modular way, building
parsers by combining parser functions. Parser functions can either succeed or
fail.


Examples
======

```elm
{-| Parse a optional sign, succeeds with a -1 if it matches a minus `Char`, otherwise it returns 1 -}
sign : Parser Int
sign =
    let
        minus =
            map (always -1) (symbol '-')
        plus =
            map (always 1) (symbol '+')
    in
        optional (plus `or` minus) 1

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


{-| Parse an integer with optional sign -}
integer : Parser Int
integer =
    map (*) sign
    |> andMap natural
```
