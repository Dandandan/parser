import Check (..)
import Random (..)
import Parser (..)
import Parser.Char as PC
import Text (plainText)
import List
import Random.Char (lowerCaseLatin, upperCaseLatin)
import String

tests =
  check [
          property "Digit parsing "
            (\number ->
                case parseString PC.digit (toString number) of
                    Err _ -> False
                    Ok i -> List.take 1 i == [number]
            ) (int 0 9),

          property "Natural parsing "
            (\number ->
                case parseString PC.natural (toString number) of
                    Err _ -> False
                    Ok i -> List.take 1 i == [number]
            ) (int 0 1000000),
          property "Integer parsing "
            (\number ->
                case parseString PC.integer (toString number) of
                    Err _ -> False
                    Ok i -> List.take 1 i == [number]
            ) (int -1000000 1000000),

          property "Float parsing "
            (\number ->
                case parseString PC.float (toString number) of
                    Err _ -> False
                    Ok i -> List.take 1 i == [number]

            ) (float -1000000 1000000),

          property "Lower parsing "
            (\char ->
                case parseString PC.lower (String.fromChar char) of
                    Err _ -> False
                    Ok i  -> True
            ) lowerCaseLatin,

          property "Lower parsing "
            (\char ->
                case parseString PC.lower (String.fromChar char) of
                    Err _ -> True
                    Ok i  -> False
            ) upperCaseLatin,

        property "Upper parsing "
            (\char ->
                case parseString PC.upper (String.fromChar char) of
                    Err _ -> False
                    Ok i  -> True
            ) upperCaseLatin,
        property "Upper parsing "
            (\char ->
                case parseString PC.upper (String.fromChar char) of
                    Err _ -> True
                    Ok i  -> False
            ) lowerCaseLatin

  ]

main = plainText tests
