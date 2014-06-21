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
empty : Parser i r
empty = always []

{-| Parses a symbol -}
symbol : a -> Parser a a
symbol = satisfy . (==)

{-| Parses a token of symbols -}
token : [a] -> Parser a [a]
token xs     =
    case xs of
        []      -> succeed []
        (x::xs) -> (::) <$> symbol x <*> token xs

{-| Combine a list of parsers -}
choice : [Parser a r] -> Parser a r
choice = foldr (<|>) empty

{-| Parses an optional element -}
optional : Parser a r -> r -> Parser a r
optional p x = p <|> succeed x

{-| Parses zero or more occurences of a parser -}
many : Parser a r -> Parser a [r]
many p xs = --(::) <$> p <*> many p <|> succeed [] (lazy version)
    case p xs of
        [] -> succeed [] xs
        _ -> ((::) <$> p <*> many p) xs

{-| Parses one or more occurences of a parser -}
some : Parser a r -> Parser a [r]
some p = (::) <$> p <*> many p

{-| Map a function over the result of the parser -}
map : (r -> s) -> Parser a r -> Parser a s
map = (<$>)

{-| Choice between two parsers -}
or : Parser a r -> Parser a r -> Parser a r
or = (<|>)

{-| Sequence two parsers -}
and : Parser a (r -> s) -> Parser a r -> Parser a s
and = (<*>)

{-| Choice between two parsers -}
(<|>) : Parser a r -> Parser a r -> Parser a r
(<|>) p q xs = p xs ++ q xs

{-| Map a function over the result of the parser -}
(<$>) : (r -> s) -> Parser a r -> Parser a s
(<$>) f p = List.map (\(r,ys) -> (f r, ys)) . p

{-| Sequence two parsers 

    data Date = Date Int Int Int
    Date <$> year <*> month <*> day

-}
(<*>) : Parser a (r -> s) -> Parser a r -> Parser a s
(<*>) p q xs = 
    let a = p xs
        b = concat <| List.map (\(_,ys) -> q ys) a
    in zipWith (\(f, ys) (b, zs) -> (f b, zs)) a b

{-| Variant of `<$>` that ignores the result of the parser -}
(<$) : r -> Parser a b -> Parser a r
f <$ p = always f <$> p

{-| Variant of `<*>` that ignores the result of the parser at the right -}
(<*) : Parser a r -> Parser a s -> Parser a r
p <* q = always <$> p <*> q

{-| Variant of `<*>` that ignores the result of the parser at the left -}
(*>) : Parser a s -> Parser a r -> Parser a r
p *> q = flip always <$> p <*> q

{-| Parses a sequence of the first parser, seperated by the second parser -} 
seperatedBy : Parser a r -> Parser a s -> Parser a [r]
seperatedBy p s = (::) <$> p <*> many (s *> p)

{-| Succeeds when input is empty -}
end : Parser a ()
end xs = case xs of
    [] -> succeed () xs
    _  -> []

infixl 4 <*>
infixr 3 <|>
infixl 4 <$>
infixl 4 <$
infixl 4 <*
infixl 4 *>
