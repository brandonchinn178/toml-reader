# toml-reader

[![](https://img.shields.io/github/workflow/status/brandonchinn178/toml-reader/CI/main)](https://github.com/brandonchinn178/toml-reader/actions)
[![](https://img.shields.io/codecov/c/gh/brandonchinn178/toml-reader)](https://app.codecov.io/gh/brandonchinn178/toml-reader)
[![](https://img.shields.io/hackage/v/toml-reader)](https://hackage.haskell.org/package/toml-reader)

TOML format parser compliant with [v1.0.0](https://toml.io/en/v1.0.0) (verified with the [`toml-test`](https://github.com/BurntSushi/toml-test) tool).

## Usage

```hs
data MyConfig = MyConfig
  { field1 :: Int
  , field2 :: Bool
  }

instance DecodeTOML MyConfig where
  tomlDecoder =
    MyConfig
      <$> getField "field1"
      <*> getField "field2"

main :: IO ()
main = do
  result <- decodeFile "config.toml"
  case result of
    Right cfg -> print (cfg :: MyConfig)
    Left e -> print e
```

## Design decisions

* Only supports reading, not writing, since TOML is lossy. For example, a simple `a.b.c = 1` line could be written in a number of ways:

    ```toml
    a.b.c = 1
    a.b = { c = 1 }
    a = { b.c = 1 }

    [a]
    b.c = 1
    b = { c = 1 }

    [a.b]
    c = 1
    ```

    Since reading/writing isn't an idempotent operation, this library won't even pretend to provide `DecodeTOML`/`EncodeTOML` typeclasses that imply that they're inverses of each other.

    Hopefully some other `toml-writer` library may come along to make it easy to specify how to format your data in TOML (e.g. a combinator for `table` vs `inlineTable`), or you could use [`tomland`](https://github.com/kowainik/tomland).

* This library defines `DecodeTOML` with an opaque `Decoder a` as opposed to a `Value -> DecodeM a` function, like `aeson` does. In my opinion, this makes the common case of decoding config files much more straightforward, especially around nested fields, which are much more common in TOML than JSON. e.g.

    ```hs
    -- aeson-like
    instance DecodeTOML MyConfig where
      decodeTOML :: Value -> DecodeM MyConfig
      decodeTOML = withObject "MyConfig" $ \o ->
        MyConfig
          <$> o .: "field1"
          <*> (o .: "field2" >>= (.: "field3"))
    ```

    ```hs
    -- with toml-parser
    instance DecodeTOML MyConfig where
      tomlDecoder :: Decoder MyConfig
      tomlDecoder =
        MyConfig
          <$> getField "field1"
          <*> getFields ["field2", "field3"]
    ```

    It also makes it easy to define ad-hoc decoders:

    ```hs
    instance DecodeTOML MyConfig where
      tomlDecoder = ...

    alternativeDecoder :: Decoder MyConfig
    alternativeDecoder = ...

    -- uses tomlDecoder
    decode "a = 1"

    -- uses explicit decoder
    decodeWith alternativeDecoder "a = 1"
    ```

    As a bonus, it also makes for a less point-free interface when defining a decoder based on another decoder, which is kinda cool:

    ```hs
    -- aeson-like
    instance DecodeTOML MyString where
      decodeTOML = fmap toMyString . decodeTOML
    ```

    ```hs
    -- with toml-parser
    instance DecodeTOML MyString where
      tomlDecoder = toMyString <$> tomlDecoder
    ```

    Ultimately, `Decoder` is just a newtype around `Value -> DecodeM a`, so we could always go back to it. Originally, I wanted to do something like [`jordan`](https://hackage.haskell.org/package/jordan), where this interface is required due to the way it parses and deserializes at the same time, but this isn't possible with TOML due to the way TOML needs to be normalized.
