**Parser**: Parser Combinator for Elm
======

An parser combinator for Elm.
Parsers convert text sequences into a data structure, for example
for handling user input and compiling programming languages.

With parser combinators you can build parsers in a modular way, building
parsers by combining parser functions.


Examples
======

```
{-| Parse a optional sign, succeeds with a -1 if it matches a minus `Char`, otherwise it returns 1 -}
sign : Parser Int
sign =
    let plus = always (-1) `map` symbol '-'
        min  = always 1 `map` symbol '+'
    in  optional (plus `or` min) 1
```

```
{-| Parse a optional sign, succeeds with a -1 if it matches a minus `Char`, otherwise it returns 1 -}
sign : Parser Int
sign =
    let plus = always (-1) `map` symbol '-'
        min  = always 1 `map` symbol '+'
    in  optional (plus `or` min) 1
```
