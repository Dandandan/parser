import Parser
import Parser (..)
import Parser.Char (..)
import Char (..)
import String

data Date = Date Int Int Int
p = integer <* symbol '/'
parses = 
    [ parse ((symbol 2) <|> (symbol 1)) [1] |> show
    , parseString digit "1" |> show
    , parseString (satisfy isDigit) "1" |> show
    , parseString (token (String.toList "123")) "123" |> show
    , parseString (many (symbol '1') <* end) "111" |> show
    , parseString (integer) "-100" |> show
    , parseString (Date `Parser.map` p `Parser.and` p `Parser.and` integer) "11/12/2012" |> show
    ]


main = asText parses
