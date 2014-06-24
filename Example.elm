import Parser
import List
import Parser (..)
import Parser.Char (..)
import Char (..)
import String

data Date = Date Int Int Int
p = integer <* symbol '/'
parses =
    [ parse ((symbol 2) <|> (symbol 1)) [1] |> show |> plainText
    , parseString digit "1" |> show |> plainText
    , parseString (satisfy isDigit) "1" |> show |> plainText
    , parseString (token (String.toList "123")) "123" |> show |> plainText
    , parseString (many (symbol '1') <* end) "111" |> show |> plainText
    , parseString integer "-100" |> show |> plainText
    , parseString float "1.2154" |> show |> plainText 
    , parseString (Date `map` p `and` p `and` integer) "11/12/2012" |> show |> plainText
    , parseString (symbol 'x' <* end) "x1231x" |> show |> plainText
    ]


main = flow down parses
