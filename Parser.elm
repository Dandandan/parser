module Parser
    ( Parser
    , parse, parseAll
    , map, or, and, andThen
    , succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end
    , recursively
    , (<*>), (<$>), (<|>), (<*), (*>), (<$)
    ) where

{-| A simple parser combinator library.

#Running the parser
@docs parse, parseAll

#Core functions
@docs map, or, and, andThen

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end

#Writing recursive grammars
@docs recursively

#Core functions (infix operators)
@docs (<*>), (<$>), (<|>), (<*), (*>), (<$)

-}

import String (cons, uncons)
import List
import Lazy (..)

type Parser result = Direct (String -> List (result, String)) | Delayed (Lazy (String -> List (result, String)))

funP : Parser result -> String -> List (result, String)
funP p =
    case p of
        Direct f  -> f
        Delayed d -> force d

{-| For realizing otherwise inexpressible recursive grammars. For
example, while

    bbbba = (symbol 'a') `or` (symbol 'b' *> bbbba)

will fail at runtime with a non-termination issue, the replacement

    bbbba = (symbol 'a') `or` (symbol 'b' *> recursively (\() -> bbbba))

is safe.
-}
recursively : (() -> Parser result) -> Parser result
recursively t = Delayed << lazy <| \() -> funP (t ())

{-| Parse a `String` using a parser, return first result -}
parse : Parser result -> String -> Result String result
parse p xs =
    case funP p xs of
        [] -> Err "parse error"
        (x::_)-> Ok (fst x)

{-| Parse a `String` using a parser, return list of results -}
parseAll : Parser result -> String -> Result String (List result)
parseAll p xs =
    case funP p xs of
        [] -> Err "parse error"
        xs -> Ok (List.map fst xs)

{-| Parser that always succeeds without consuming input -}
succeed : result -> Parser result
succeed b =
    Direct <| \xs -> [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (Char -> Bool) -> Parser Char
satisfy p = Direct <| \xs ->
    case uncons xs of
        Nothing -> []
        Just (x, xs') -> if p x then [(x, xs')] else []

{-| Parser that always fails -}
empty : Parser result
empty =
    Direct <| always []

{-| Parses a symbol -}
symbol : Char -> Parser Char
symbol x =
    satisfy (\s -> s == x)

{-| Parses a token of symbols -}
token : String -> Parser String
token xs =
    case (uncons xs) of
        Nothing      -> succeed ""
        Just (x,xs)  -> cons `map` symbol x `and` token xs

{-| Combine a list of parsers -}
choice : List (Parser result) -> Parser result
choice =
    List.foldr or empty

{-| Parses an optional element -}
optional : Parser result -> result -> Parser result
optional p x =
    p `or` succeed x

{-| Parses zero or more occurences of a parser -}
many : Parser result -> Parser (List result)
many p =
    Direct <| \xs ->
        case funP p xs of
            [] -> funP (succeed []) xs
            _ -> funP (map (::) p `and` many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser result -> Parser (List result)
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser
      -- Counts the amount of digits
      count : Parser Int
      count = length `map` many digit

-}
map : (result -> result2) -> Parser result -> Parser result2
map f p =
    Direct <| \xs -> List.map (\(r,ys) -> (f r, ys)) <| funP p xs

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser result -> Parser result -> Parser result
or p q =
    Direct <| \xs -> funP p xs ++ funP q xs

{-| Sequence two parsers

    type Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser (result -> result2) -> Parser result -> Parser result2
and p q =
    Direct <| \xs ->
        List.concat << List.map (\(f, ys) -> List.map (\(r, rs) -> (f r, rs)) <| funP q ys) <| funP p xs

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.
-}
andThen : Parser result -> (result -> Parser result2) -> Parser result2
andThen p f =
    Direct <| \xs ->
        List.concat << List.map (\(y,ys) -> funP (f y) ys) <| funP p xs

{-| Parses a sequence of the first parser, separated by the second parser -}
separatedBy : Parser result -> Parser result2 -> Parser (List result)
separatedBy p s =
    map (::) p `and` many (map (\x y -> y) s `and` p)

{-| Succeeds when input is empty -}
end : Parser ()
end =
    Direct <| \xs -> case xs of
        "" -> funP (succeed ()) xs
        _  -> []

{-| Choice between two parsers -}
(<|>) : Parser result -> Parser result -> Parser result
(<|>) = or

{-| Map a function over the result of the parser -}
(<$>) : (result -> result2) -> Parser result -> Parser result2
(<$>) = map

{-| Sequence two parsers -}
(<*>) : Parser (result -> result2) -> Parser result -> Parser result2
(<*>) = and

{-| Variant of `<$>` that ignores the result of the parser -}
(<$) : result -> Parser x -> Parser result
f <$ p =
    always f `map` p

{-| Variant of `<*>` that ignores the result of the parser at the right -}
(<*) : Parser result -> Parser x -> Parser result
p <* q =
    always `map` p `and` q

{-| Variant of `<*>` that ignores the result of the parser at the left -}
(*>) : Parser x -> Parser result -> Parser result
p *> q =
    flip always `map` p `and` q

infixl 4 <*>
infixl 4 `and`
infixr 3 <|>
infixr 3 `or`
infixl 4 <$>
infixl 4 `map`
infixl 4 <$
infixl 4 <*
infixl 4 *>
