## 6.2.0 - 2015-08-22
## Added
* `Parser.Char.singleQuoted`

## 6.1.0 - 2015-08-22
## Added
* `Parser.andMap` (`Parser.andMap` with reversed arguments)

## Fixed
* `Parser.separatedBy`

## [6.0.0] - 2015-02-25
## Removed
* `<$>`, `<|>` and `<*>` (use `Parser.map`, `Parser.or` and `Parser.and`)


## [5.0.4] - 2015-02-25
### Added
* Quoted parser (thanks to @jivagoalves)

### Changed
* Updated to elm-check3 and enabled tests again (thanks to @jivagoalves)

### Fixed
* Fixed `Parser.Number.float` issue found by test (thanks again @jivagoalves)


## [5.0.2] - 2015-02-25
### Changed
* Update to Elm 0.15


## [5.0.0] - 2015-02-25
### Changed
* Parse now returns only the first matching parser, use `Parser.parseAll` to get
  results of all matching parsers

## [4.1.0] - 2015-02-16
### Fixed
* Exported sign parser
* Documentation improvement

## [4.0.0] - 2015-02-16
### Changed
* Simplified parser by specialising it to Strings
* Splitting out number parsers
* Remove parser record and parseString function

## [3.1.0] - 2015-02-15
* Added tests
* Fixed float parser
