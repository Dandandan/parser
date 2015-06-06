import Check exposing (..)
import Check.Investigator exposing ( Investigator
                                   , investigator
                                   , rangeInt
                                   , float
                                   , char
                                   , lowerCaseChar
                                   , upperCaseChar
                                   )
import Check.Runner.Browser exposing (display)
import Parser exposing (parse)
import Parser.Char as PC
import Parser.Number as PN
import Result.Extra exposing (isOk)
import Shrink
import String

parserSuite =
  suite "Parser"
    [ claim
        "Digit parsing"
      `that`
        (parse PN.digit << toString)
      `is`
        Ok
      `for`
        (rangeInt 0 9)
    , claim
        "Natural parsing"
      `that`
        (parse PN.natural << toString)
      `is`
        Ok
      `for`
        (rangeInt 0 1000000)
    , claim
        "Integer parsing"
      `that`
        (parse PN.integer << toString)
      `is`
        Ok
      `for`
        (rangeInt -1000000 1000000)
    , claim
        "Float parsing"
      `that`
        (parse PN.float << (\n -> let s = toString n
                                  in if String.contains "." s
                                        then s
                                        else s ++ ".0"))
      `is`
        Ok
      `for`
        float
    , claim
        "Lower parsing"
      `true`
        (isOk << parse PC.lower << String.fromChar)
      `for`
        lowerCaseChar
    , claim
        "Lower parsing"
      `false`
        (isOk << parse PC.lower << String.fromChar)
      `for`
        upperCaseChar
    , claim
        "Upper parsing"
      `true`
        (isOk << parse PC.upper << String.fromChar)
      `for`
        upperCaseChar
    , claim
        "Upper parsing"
      `false`
        (isOk << parse PC.upper << String.fromChar)
      `for`
        lowerCaseChar
    ]

result = quickCheck parserSuite

main = display result
