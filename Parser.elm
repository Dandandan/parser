module Parser where

{-| A simple parser combinator library.

#Running the parser
@docs parse, parseString, parser

#Core functions
@docs map, or, and, andThen

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, seperatedBy, end

#Core functions (infix operators)
@docs (<*>), (<$>), (<|>), (<*), (*>), (<$)
-}

import String
import Result (..)
import List
import List (..)

type alias Parser a r = List a -> List (r, List a)

{-| Parse a list using a parser, return list of results -}
parse : Parser a r -> List a -> Result String (List r)
parse p xs =
  case p xs of
    ([],_) -> Err "parse error"
    (xs,_) -> Ok xs

{-| The parser record makes things look nicer when using command syntax -}
parser : { andThen : Parser s a -> (a -> Parser s b) -> Parser s b }
parser = { andThen = andThen }

{-| Parse a `String` using a `Char` parser  -}
parseString : Parser Char r -> String -> Result String (List r)
parseString p = parse p << String.toList

{-| Parser that always succeeds without consuming input -}
succeed : r -> Parser a r
succeed b xs = [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (a -> Bool) -> Parser a a
satisfy p xs =
  case xs of
    [] -> []
    (x::xs') -> if p x then [(x, xs')] else []

{-| Parser that always fails -}
empty : Parser a r
empty = always []

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
many p xs = --(::) <$> p <*> many p <|> succeed [] (lazy version)
    case p xs of
        [] -> succeed [] xs
        _ -> ((::) `map` p `and` many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser a r -> Parser a (List r)
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser

      count = length `map` many digit

-}
map : (r -> s) -> Parser a r -> Parser a s
map f p = List.map (\(r,ys) -> (f r, ys)) << p

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser a r -> Parser a r -> Parser a r
or p q xs = p xs ++ q xs

{-| Sequence two parsers

    data Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser a (r -> s) -> Parser a r -> Parser a s
and p q =
    concat << List.map (\(f, ys) -> List.map (\(r, rs) -> (f r, rs)) <| q ys) << p

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.
-}
andThen : Parser s a -> (a -> Parser s b) -> Parser s b
andThen p f = concat << List.map (\(y,ys) -> f y ys) << p

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
end xs = case xs of
    [] -> succeed () xs
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
