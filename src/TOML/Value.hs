{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TOML.Value (
  Value (..),
  renderValue,
  Table,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, TimeZone)

type Table = Map Text Value

data Value
  = Table Table
  | Array [Value]
  | String Text
  | Integer Integer
  | Float Double -- TOML spec specifies this must be a double precision float: https://github.com/toml-lang/toml/issues/538
  | Boolean Bool
  | OffsetDateTime (LocalTime, TimeZone)
  | LocalDateTime LocalTime
  | LocalDate Day
  | LocalTime TimeOfDay
  deriving (Show, Eq)

-- | Render a Value in pseudo-JSON format.
renderValue :: Value -> Text
renderValue = \case
  Table kvs -> "{" <> Text.intercalate ", " (map renderKeyValue $ Map.toList kvs) <> "}"
  Array vs -> "[" <> Text.intercalate ", " (map renderValue vs) <> "]"
  String s -> showT s
  Integer x -> showT x
  Float x -> showT x
  Boolean b -> if b then "true" else "false"
  OffsetDateTime x -> showT x
  LocalDateTime x -> showT x
  LocalDate x -> showT x
  LocalTime x -> showT x
  where
    renderKeyValue (k, v) = showT k <> ": " <> renderValue v

    showT :: Show a => a -> Text
    showT = Text.pack . show
