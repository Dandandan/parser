module Parser exposing
    ( Parser
    , parse, parseAll
    , map, or, andMap, andThen, and
    , succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end
    , recursively
    , (<*), (*>), (<$)
    )

{-| A simple parser combinator library.

@docs Parser

#Running the parser
@docs parse, parseAll

#Core functions
@docs map, or, andMap, andThen, and

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end

#Writing recursive grammars
@docs recursively

#Core functions (infix operators)
@docs (<*), (*>), (<$)

-}

import String
import List
import Lazy as L

{-| Parser type
-}
type Parser result = Direct (String -> List (result, String)) | Delayed (L.Lazy (String -> List (result, String)))

funP : Parser result -> String -> List (result, String)
funP p =
    case p of
        Direct f ->
            f
        Delayed d ->
            L.force d

{-| For realizing otherwise inexpressible recursive grammars. For
example, while

    bbbba = or (symbol 'a') (symbol 'b' *> bbbba)

will fail at runtime with a non-termination issue, the replacement

    bbbba = or (symbol 'a') (symbol 'b' *> recursively (\() -> bbbba))

is safe.
-}
recursively : (() -> Parser result) -> Parser result
recursively t =
    Delayed << L.lazy <| \() ->
        funP (t ())

{-| Parse a `String` using a parser, return first result -}
parse : Parser result -> String -> Result String result
parse p xs =
    case funP p xs of
        [] ->
            Err "parse error"
        (x :: _) ->
            Ok (Tuple.first x)

{-| Parse a `String` using a parser, return list of results -}
parseAll : Parser result -> String -> Result String (List result)
parseAll p xs =
    case funP p xs of
        [] ->
            Err "parse error"
        xs ->
            Ok (List.map Tuple.first xs)

{-| Parser that always succeeds without consuming input -}
succeed : result -> Parser result
succeed b =
    Direct <| \xs ->
        [(b, xs)]

{-| Parser that satisfies a given predicate -}
satisfy : (Char -> Bool) -> Parser Char
satisfy p = Direct <| \xs ->
    case String.uncons xs of
        Nothing ->
            []
        Just (x, xs1) ->
            if p x then [(x, xs1)] else []

{-| Parser that always fails -}
empty : Parser result
empty =
    Direct (always [])

{-| Parses a symbol -}
symbol : Char -> Parser Char
symbol x =
    satisfy (\s -> s == x)

{-| Parses a token of symbols -}
token : String -> Parser String
token xs =
    case String.uncons xs of
        Nothing ->
            succeed ""
        Just (x, xs) ->
            symbol x
            |> map String.cons
            |> andMap (token xs)

{-| Combine a list of parsers -}
choice : List (Parser result) -> Parser result
choice =
    List.foldr or empty

{-| Parses an optional element -}
optional : Parser result -> result -> Parser result
optional p x =
    or p (succeed x)

{-| Parses zero or more occurences of a parser -}
many : Parser result -> Parser (List result)
many p =
    Direct <| \xs ->
        case funP p xs of
            [] ->
                funP (succeed []) xs
            _ ->
                funP (some p) xs

{-| Parses one or more occurences of a parser -}
some : Parser result -> Parser (List result)
some p =
    map (::) p
    |> andMap (many p)

{-| Map a function over the result of the parser
      -- Counts the amount of digits
      count : Parser Int
      count = map length (many digit)

-}
map : (result -> result2) -> Parser result -> Parser result2
map f p =
    Direct <| \xs ->
        funP p xs
          |> List.map (\(r, ys) -> (f r, ys))

{-| Choice between two parsers

      oneOrTwo = or (symbol '1') (symbol '2')
-}
or : Parser result -> Parser result -> Parser result
or p q =
    Direct <| \xs ->
        funP p xs ++ funP q xs

{-| Sequence two parsers

    type Date = Date Int Int Int
    date =
        map Date year
        |> andMap month
        |> andMap day
-}
andMap : Parser result -> Parser (result -> result2) -> Parser result2
andMap q p =
    Direct <| \xs ->
        funP p xs
        |> List.map (\(f, ys) -> funP q ys |> List.map (\(r, rs) -> (f r, rs)))
        |> List.concat


{-| Sequence two parsers

    type Date = Date Int Int Int
    date = and (and (map Date year) month) day

-}
and : Parser (result -> result2) -> Parser result -> Parser result2
and p q =
    p
    |> andMap q

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.

    tag = tagLiteral 
        |> andThen (openTag)
-}
andThen : (result -> Parser result2) -> Parser result -> Parser result2
andThen f p =
    Direct <| \xs ->
        funP p xs
        |> List.map (\(y, ys) -> funP (f y) ys)
        |> List.concat

{-| Parses a sequence of the first parser, separated by the second parser
```
naturals = separatedBy Number.natural (symbol ',')
```
 -}
separatedBy : Parser result -> Parser result2 -> Parser (List result)
separatedBy p s =
    map (::) p
    |> andMap (many (map (\x y -> y) s |> andMap p))
    |> (flip or) (succeed [])

{-| Succeeds when input is empty -}
end : Parser ()
end =
    Direct <| \xs -> case xs of
        "" ->
            funP (succeed ()) xs
        _  ->
            []

{-| Variant of `map` that ignores the result of the parser -}
(<$) : result -> Parser x -> Parser result
(<$) f p =
    map (always f) p

{-| Variant of `and` that ignores the result of the parser at the right -}
(<*) : Parser result -> Parser x -> Parser result
(<*) p q =
    map always p
    |> andMap q

{-| Variant of `and` that ignores the result of the parser at the left -}
(*>) : Parser x -> Parser result -> Parser result
(*>) p q =
    map (flip always) p
    |> andMap q

--infixl 4 and
--infixr 3 or
--infixl 4 map
