import Parser
import List
import Parser (..)
import Parser.Char (..)
import Char (..)
import String
import Text (..)
import Graphics.Element (..)

type Date = Date Int Int Int
p = integer <* symbol '/'
parses =
    [ parse ((symbol 2) <|> (symbol 1)) [1] |> toString |> plainText
    , parseString digit "1" |> toString |> plainText
    , parseString (satisfy isDigit) "1" |> toString |> plainText
    , parseString (token (String.toList "123")) "123" |> toString |> plainText
    , parseString (many (symbol '1') <* end) "111" |> toString |> plainText
    , parseString integer "-100" |> toString |> plainText
    , parseString float "1.2154" |> toString |> plainText
    , parseString (Date `Parser.map` p `and` p `and` integer) "11/12/2012" |> toString |> plainText
    , parseString (symbol 'x' <* end) "x2" |> toString |> plainText
      -- should fail
    , parseString ((symbol 'x' <|> symbol 'y') `andThen` (\r -> if r == 'x' then symbol 'x'  else symbol 'y' )) "xy" |> toString |> plainText
    ]

main = flow down parses
