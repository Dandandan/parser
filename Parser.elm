module Parser where

{-| A simple parser combinator library.

#Running the parser
@docs parse, parseString

#Core functions
@docs map, or, and

#Combinators
@docs succeed, satisfy, empty, expect, symbol, token, choice, optional, many, some, seperatedBy, end

#Core functions (infix operators)
@docs (<*>), (<$>), (<|>), (<*), (*>), (<$)
-}

import String
import Either (..)
import List

data Result a r
  = Succeed [(r, [a])]
  | Expect String [a]
  | Fail                

type Parser a r = [a] -> Result a r

{-| Parse a list using a parser -}
parse : Parser a r -> [a] -> Either String r
parse p xs =
  case p xs of
    Succeed ((e, _)::_) -> Right e
    Expect  e (x::_)    -> Left ("unexpected " ++ show x ++ ", expected " ++ e)
    Expect  e []        -> Left ("error: " ++ e)
    _                   -> Left "parse error"

{-| Parse a `String` using a `Char` parser  -}
parseString : Parser Char r -> String -> Either String r
parseString p = parse p . String.toList

{-| Parser that always succeeds without consuming input -}
succeed : r -> Parser a r
succeed b xs = Succeed [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (a -> Bool) -> Parser a a
satisfy p xs = 
  case xs of
    [] -> Fail
    (x::xs') -> if p x then Succeed [(x, xs')] else Fail

{-| Parser that always fails -}
empty : Parser a r
empty = always <| Fail

{-| Parses a symbol -}
symbol : a -> Parser a a
symbol x = satisfy (\s -> s == x) `expect` (show x)

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

{-| Adds a message in case parsing fails

      aOrB = symbol 'a' `or` symbol 'b' `expect` "a or b"

-}
expect : Parser a r -> String -> Parser a r
expect p s = p `or` Expect s

{-| Parses zero or more occurences of a parser -}
many : Parser a r -> Parser a [r]
many p xs = --(::) <$> p <*> many p <|> succeed [] (lazy version)
    case p xs of
        Succeed _ -> ((::) `map` p `and` many p) xs
        _         -> succeed [] xs

{-| Parses one or more occurences of a parser -}
some : Parser a r -> Parser a [r]
some p = (::) `map` p `and` many p

{-| Map a function over the result of the parser 

      count = length `map` many digit

-}
map : (r -> s) -> Parser a r -> Parser a s
map f p = listToR . List.map (\(r,ys) -> (f r, ys)) . rToList . p

rToList r =
  case r of
    Succeed s -> s
    _         -> []

listToR xs =
  case xs of
    [] -> Fail
    _  -> Succeed xs

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser a r -> Parser a r -> Parser a r
or p q xs = 
  case (p xs, q xs) of
    (Succeed a, Succeed b)   -> Succeed (a ++ b)
    (Succeed a, _)           -> Succeed a
    (_, Succeed b)           -> Succeed b
    (Expect a _, Expect b _) -> Expect (a ++ " or " ++ b) xs
    (Expect a _ , _)         -> Expect a xs
    (_, Expect b _)          -> Expect b xs
    _                        -> Fail


isExpect r = case r of
    Expect _ _ -> True
    _          -> False

{-| Sequence two parsers 

    data Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser a (r -> s) -> Parser a r -> Parser a s
and p q xs =
  let a = p xs
      a' = rToList a
      b = List.map (\(_,ys) -> q ys) a'
      b' = concat <| List.map rToList b
  in
    case (zipWith (\(f, ys) (b, zs) -> (f b, zs)) a' b') of
      [] -> case a of
          Expect e xs        -> Expect e xs
          Succeed ((r,a)::_) -> 
            case filter isExpect b of
              Expect e es :: _  -> Expect e es
              _                 -> Fail
          _                     -> Fail
      x  -> listToR x

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

{-| Parses a sequence of the first parser, seperated by the second parser -} 
seperatedBy : Parser a r -> Parser a s -> Parser a [r]
seperatedBy p s = (::) `map` p `and` many (s *> p)

{-| Succeeds when input is empty -}
end : Parser a ()
end xs = case xs of
    [] -> succeed () xs
    _  -> Fail

infixl 4 <*>
infixl 4 `and`
infixr 3 <|>
infixr 3 `or`
infixr 3 `expect`
infixl 4 <$>
infixl 4 `map`
infixl 4 <$
infixl 4 <*
infixl 4 *>
