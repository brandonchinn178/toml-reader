## Unreleased

## v0.3.0.0

* Drop support for GHC < 9.8
* Fix compatibility with toml-test 1.5.0
    * Allow specifying new table section in dotted table path
    * Error when setting dotted path inside array

## v0.2.2.0

* Add support for GHC 9.10 + 9.12
* Fix context when decoding `Map`
* Add `getTableOf`

## v0.2.1.0

* Drop support for GHC < 9.0

## v0.2.0.0

* Add getFieldOr [#10](https://github.com/brandonchinn178/toml-reader/issues/10)
* Handle extremely large float values [#8](https://github.com/brandonchinn178/toml-reader/issues/8)
* Add Exception instance for TOMLError

## v0.1.0.0

Initial release
