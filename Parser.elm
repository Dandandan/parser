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
import Result (..)
import List
import List (..)
import Lazy (..)

type Parser a r = Direct (List a -> List (r, List a)) | Delayed (Lazy (List a -> List (r, List a)))

funP : Parser a r -> List a -> List (r, List a)
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
recursively : (() -> Parser a r) -> Parser a r
recursively t = Delayed << lazy <| \() -> funP (t ())

{-| Parse a list using a parser, return list of results -}
parse : Parser a r -> List a -> Result String (List r)
parse p xs =
  case funP p xs of
    [] -> Err "parse error"
    xs -> Ok (List.map fst xs)

{-| The parser record makes things look nicer when using command syntax -}
parser : { andThen : Parser s a -> (a -> Parser s b) -> Parser s b }
parser = { andThen = andThen }

{-| Parse a `String` using a `Char` parser  -}
parseString : Parser Char r -> String -> Result String (List r)
parseString p = parse p << String.toList

{-| Parser that always succeeds without consuming input -}
succeed : r -> Parser a r
succeed b = Direct <| \xs -> [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (a -> Bool) -> Parser a a
satisfy p = Direct <| \xs ->
  case xs of
    [] -> []
    (x::xs') -> if p x then [(x, xs')] else []

{-| Parser that always fails -}
empty : Parser a r
empty = Direct <| always []

{-| Parses a symbol -}
symbol : a -> Parser a a
symbol x = satisfy (\s -> s == x)

{-| Parses a token of symbols -}
token : List a -> Parser a (List a)
token xs     =
  case xs of
    []      -> succeed []
    (x::xs) -> (::) `map` symbol x `and` token xs

{-| Combine a list of parsers -}
choice : List (Parser a r) -> Parser a r
choice = foldr or empty

{-| Parses an optional element -}
optional : Parser a r -> r -> Parser a r
optional p x = p `or` succeed x

{-| Parses zero or more occurences of a parser -}
many : Parser a r -> Parser a (List r)
many p = --(::) <$> p <*> many p <|> succeed [] (lazy version)
  Direct <| \xs ->
    case funP p xs of
        [] -> funP (succeed []) xs
        _ -> funP ((::) `map` p `and` many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser a r -> Parser a (List r)
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser

      count = length `map` many digit

-}
map : (r -> s) -> Parser a r -> Parser a s
map f p = Direct <| \xs -> List.map (\(r,ys) -> (f r, ys)) <| funP p xs

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser a r -> Parser a r -> Parser a r
or p q = Direct <| \xs -> funP p xs ++ funP q xs

{-| Sequence two parsers

    data Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser a (r -> s) -> Parser a r -> Parser a s
and p q = Direct <| \xs -> 
    concat << List.map (\(f, ys) -> List.map (\(r, rs) -> (f r, rs)) <| funP q ys) <| funP p xs

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.
-}
andThen : Parser s a -> (a -> Parser s b) -> Parser s b
andThen p f = Direct <| \xs -> concat << List.map (\(y,ys) -> funP (f y) ys) <| funP p xs

{-| Choice between two parsers -}
(<|>) : Parser a r -> Parser a r -> Parser a r
(<|>) = or

{-| Map a function over the result of the parser -}
(<$>) : (r -> s) -> Parser a r -> Parser a s
(<$>) = map

{-| Sequence two parsers

-}
(<*>) : Parser a (r -> s) -> Parser a r -> Parser a s
(<*>) = and

{-| Variant of `<$>` that ignores the result of the parser -}
(<$) : r -> Parser a b -> Parser a r
f <$ p = always f `map` p

{-| Variant of `<*>` that ignores the result of the parser at the right -}
(<*) : Parser a r -> Parser a s -> Parser a r
p <* q = always `map` p `and` q

{-| Variant of `<*>` that ignores the result of the parser at the left -}
(*>) : Parser a s -> Parser a r -> Parser a r
p *> q = flip always `map` p `and` q

{-| Parses a sequence of the first parser, separated by the second parser -}
separatedBy : Parser a r -> Parser a s -> Parser a (List r)
separatedBy p s = (::) `map` p `and` many (s *> p)

{-| Succeeds when input is empty -}
end : Parser a ()
end = Direct <| \xs -> case xs of
    [] -> funP (succeed ()) xs
    _  -> []

infixl 4 <*>
infixl 4 `and`
infixr 3 <|>
infixr 3 `or`
infixl 4 <$>
infixl 4 `map`
infixl 4 <$
infixl 4 <*
infixl 4 *>
