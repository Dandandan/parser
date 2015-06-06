import Check exposing (..)
import Check.Investigator exposing (Investigator, investigator, rangeInt, float, char)
import Check.Runner.Browser exposing (display)
import Parser exposing (parse)
import Parser.Char as PC
import Parser.Number as PN
import Random.Char exposing (lowerCaseLatin, upperCaseLatin)
import Result.Extra exposing (isOk)
import Shrink
import String

lowerCaseChar : Investigator Char
lowerCaseChar = investigator lowerCaseLatin Shrink.character

upperCaseChar : Investigator Char
upperCaseChar = investigator upperCaseLatin Shrink.character

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
        (parse PN.float << toString << (\n -> if n == 0 then 0.0 else n))
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
