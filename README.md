Parser
======

Functions for parsing Strings, converting them to data types.

    {- A Date parser -}
    type Date = Date Int Int Int
    date : Parser Date
    date = Date `map` year `and` month `and` day


