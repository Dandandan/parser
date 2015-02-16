module Parser
    ( Parser
    , parse, parseString, parser
    , map, or, and, andThen
    , succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end
    , recursively
    , (<*>), (<$>), (<|>), (<*), (*>), (<$)
    ) where

{-| A simple parser combinator library.

#Running the parser
@docs parse, parseString, parser

#Core functions
@docs map, or, and, andThen

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end

#Writing recursive grammars
@docs recursively

#Core functions (infix operators)
@docs (<*>), (<$>), (<|>), (<*), (*>), (<$)

-}

import String
import List
import List (..)
import Lazy (..)

type Parser input result = Direct (List input -> List (result, List input)) | Delayed (Lazy (List input -> List (result, List input)))

funP : Parser input result -> List input -> List (result, List input)
funP p = case p of
           Direct f  -> f
           Delayed d -> force d

{-| For realizing otherwise inexpressible recursive grammars. For
example, while

    bbbba = (symbol 'a') `or` (symbol 'b' *> bbbba)

will fail at runtime with a non-termination issue, the replacement

    bbbba = (symbol 'a') `or` (symbol 'b' *> recursively (\() -> bbbba))

is safe.
-}
recursively : (() -> Parser input result) -> Parser input result
recursively t = Delayed << lazy <| \() -> funP (t ())

{-| Parse a list using a parser, return list of results -}
parse : Parser input result -> List input -> Result String (List result)
parse p xs =
  case funP p xs of
    [] -> Err "parse error"
    xs -> Ok (List.map fst xs)

{-| The parser record makes things look nicer when using command syntax -}
parser : { andThen : Parser input result -> (result -> Parser input result2) -> Parser input result2 }
parser = { andThen = andThen }

{-| Parse a `String` using a `Char` parser  -}
parseString : Parser Char result -> String -> Result String (List result)
parseString p = parse p << String.toList

{-| Parser that always succeeds without consuming input -}
succeed : result -> Parser input result
succeed b = Direct <| \xs -> [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (input -> Bool) -> Parser input input
satisfy p = Direct <| \xs ->
  case xs of
    [] -> []
    (x::xs') -> if p x then [(x, xs')] else []

{-| Parser that always fails -}
empty : Parser input result
empty = Direct <| always []

{-| Parses a symbol -}
symbol : input -> Parser input input
symbol x = satisfy (\s -> s == x)

{-| Parses a token of symbols -}
token : List input -> Parser input (List input)
token xs     =
  case xs of
    []      -> succeed []
    (x::xs) -> (::) `map` symbol x `and` token xs

{-| Combine a list of parsers -}
choice : List (Parser input result) -> Parser input result
choice = foldr or empty

{-| Parses an optional element -}
optional : Parser input result -> result -> Parser input result
optional p x = p `or` succeed x

{-| Parses zero or more occurences of a parser -}
many : Parser input result -> Parser input (List result)
many p = --(::) <$> p <*> many p <|> succeed [] (lazy version)
  Direct <| \xs ->
    case funP p xs of
        [] -> funP (succeed []) xs
        _ -> funP ((::) `map` p `and` many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser input result -> Parser input (List result)
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser

      count = length `map` many digit

-}
map : (result -> result2) -> Parser input result -> Parser input result2
map f p = Direct <| \xs -> List.map (\(r,ys) -> (f r, ys)) <| funP p xs

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser input result -> Parser input result -> Parser input result
or p q = Direct <| \xs -> funP p xs ++ funP q xs

{-| Sequence two parsers

    data Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser input (result -> result2) -> Parser input result -> Parser input result2
and p q = Direct <| \xs ->
    concat << List.map (\(f, ys) -> List.map (\(r, rs) -> (f r, rs)) <| funP q ys) <| funP p xs

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.
-}
andThen : Parser input result -> (result -> Parser input result2) -> Parser input result2
andThen p f = Direct <| \xs -> concat << List.map (\(y,ys) -> funP (f y) ys) <| funP p xs


{-| Parses a sequence of the first parser, separated by the second parser -}
separatedBy : Parser input result -> Parser input result2 -> Parser input (List result)
separatedBy p s = (::) `map` p `and` many ((\x y -> y) `map` s `and` p)

{-| Succeeds when input is empty -}
end : Parser input ()
end = Direct <| \xs -> case xs of
    [] -> funP (succeed ()) xs
    _  -> []

{-| Choice between two parsers -}
(<|>) : Parser input result -> Parser input result -> Parser input result
(<|>) = or

{-| Map a function over the result of the parser -}
(<$>) : (result -> result2) -> Parser input result -> Parser input result2
(<$>) = map

{-| Sequence two parsers

--}
(<*>) : Parser input (result -> result2) -> Parser input result -> Parser input result2
(<*>) = and

{-| Variant of `<$>` that ignores the result of the parser -}
(<$) : result -> Parser input x -> Parser input result
f <$ p = always f `map` p

{-| Variant of `<*>` that ignores the result of the parser at the right -}
(<*) : Parser input result -> Parser input x -> Parser input result
p <* q = always `map` p `and` q

{-| Variant of `<*>` that ignores the result of the parser at the left -}
(*>) : Parser input x -> Parser input result -> Parser input result
p *> q = flip always `map` p `and` q

infixl 4 <*>
infixl 4 `and`
infixr 3 <|>
infixr 3 `or`
infixl 4 <$>
infixl 4 `map`
infixl 4 <$
infixl 4 <*
infixl 4 *>
