module Parser where

{-| A simple parser combinator library.

#Running the parser
@docs parse, parseString

#Core functions
@docs map, or, and

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, seperatedBy, end

#Core functions (infix operators)
@docs (<*>), (<$>), (<|>), (<*), (*>), (<$)
-}

import String
import Either (..)
import List

type Parser a r = [a] -> [(r, [a])]

{-| Parse a list using a parser -}
parse : Parser a r -> [a] -> Either String r
parse p xs =
  case p xs of
    ((e, _)::_) -> Right e
    _           -> Left "Parse Error"

{-| Parse a `String` using a `Char` parser  -}
parseString : Parser Char r -> String -> Either String r
parseString p = parse p . String.toList

{-| Parser that always succeeds without consuming input -}
succeed : r -> Parser a r
succeed b xs = [(b,xs)]

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
symbol = satisfy . (==)

{-| Parses a token of symbols -}
token : [a] -> Parser a [a]
token xs     =
    case xs of
        []      -> succeed []
        (x::xs) -> (::) `map` symbol x `and` token xs

{-| Combine a list of parsers -}
choice : [Parser a r] -> Parser a r
choice = foldr or empty

{-| Parses an optional element -}
optional : Parser a r -> r -> Parser a r
optional p x = p `or` succeed x

{-| Parses zero or more occurences of a parser -}
many : Parser a r -> Parser a [r]
many p xs = --(::) <$> p <*> many p <|> succeed [] (lazy version)
    case p xs of
        [] -> succeed [] xs
        _ -> ((::) `map` p `and` many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser a r -> Parser a [r]
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser 

      count = length `map` (many digit)

-}
map : (r -> s) -> Parser a r -> Parser a s
map f p = List.map (\(r,ys) -> (f r, ys)) . p

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser a r -> Parser a r -> Parser a r
or p q xs = p xs ++ q xs

{-| Sequence two parsers 

    data Date = Date Int Int Int
    map Date year `and` month `and` day
-}
and : Parser a (r -> s) -> Parser a r -> Parser a s
and p q xs =
    let a = p xs
        b = concat <| List.map (\(_,ys) -> q ys) a
    in zipWith (\(f, ys) (b, zs) -> (f b, zs)) a b
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
p *> q = (flip always) `map` p `and` q

{-| Parses a sequence of the first parser, seperated by the second parser -} 
seperatedBy : Parser a r -> Parser a s -> Parser a [r]
seperatedBy p s = (::) `map` p `and` many (s *> p)

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
