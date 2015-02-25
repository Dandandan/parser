import Check (..)
import Random (..)
import Parser (..)
import Parser.Char as PC
import Parser.Number as PN
import Text
import Signal
import List
import Random.Char (lowerCaseLatin, upperCaseLatin)
import String

tests =
    continuousCheck [
          property "Digit parsing "
            (\number ->
                parse PN.digit (toString number) == Ok number
            ) (int 0 9),

          property "Natural parsing "
            (\number ->
                parse PN.natural (toString number) == Ok number
            ) (int 0 1000000),

          property "Integer parsing "
            (\number ->
                parse PN.integer (toString number) == Ok number
            ) (int -1000000 1000000),

          property "Float parsing "
            (\number ->
                parse PN.float (toString number) == Ok number

            ) (float -1000000 1000000),

          property "Lower parsing "
            (\char ->
                case parse PC.lower (String.fromChar char) of
                    Err _ -> False
                    Ok i  -> True
            ) lowerCaseLatin,

          property "Lower parsing "
            (\char ->
                case parse PC.lower (String.fromChar char) of
                    Err _ -> True
                    Ok i  -> False
            ) upperCaseLatin,

        property "Upper parsing "
            (\char ->
                case parse PC.upper (String.fromChar char) of
                    Err _ -> False
                    Ok i  -> True
            ) upperCaseLatin,
        property "Upper parsing "
            (\char ->
                case parse PC.upper (String.fromChar char) of
                    Err _ -> True
                    Ok i  -> False
            ) lowerCaseLatin

  ]

main = Signal.map display tests
