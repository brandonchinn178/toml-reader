{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Internal (
  Value (..),
  renderValue,
  Table,
  TOMLError (..),
  NormalizeError (..),
  renderTOMLError,
) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import GHC.Generics (Generic)

type Table = Map Text Value

data Value
  = Table Table
  | Array [Value]
  | String Text
  | Integer Integer
  | Float Double
  | Boolean Bool
  | OffsetDateTime UTCTime
  | LocalDateTime LocalTime
  | LocalDate Day
  | LocalTime TimeOfDay
  deriving (Show, Eq, Generic, NFData)

-- | Render a Value in pseudo-JSON format.
renderValue :: Value -> Text
renderValue = \case
  Table kvs -> "{" <> Text.intercalate ", " (map renderKeyValue $ Map.toList kvs) <> "}"
  Array vs -> "[" <> Text.intercalate ", " (map renderValue vs) <> "]"
  String s -> showT s
  Integer x -> showT x
  Float x -> showT x
  Boolean b -> showT b
  OffsetDateTime x -> showT x
  LocalDateTime x -> showT x
  LocalDate x -> showT x
  LocalTime x -> showT x
  where
    renderKeyValue (k, v) = showT k <> ": " <> renderValue v

    showT :: Show a => a -> Text
    showT = Text.pack . show

data TOMLError
  = ParseError Text
  | NormalizeError NormalizeError
  deriving (Show)

data NormalizeError
  = -- | When a key is defined twice, e.g.
    --
    -- @
    -- name = 'First'
    -- name = 'Second'
    -- @
    DuplicateKeyError
      { _path :: [Text]
      , _existingValue :: Value
      , _valueToSet :: Value
      }
  | -- | When a section is defined twice, e.g.
    --
    -- @
    -- [foo]
    -- a = 1
    --
    -- [foo]
    -- b = 2
    -- @
    DuplicateSectionError
      { _sectionKey :: [Text]
      }
  | -- | When a key is already defined, but attempting to create an
    -- implicit array at the same key, e.g.
    --
    -- @
    -- list = [1, 2, 3]
    --
    -- [[list]]
    -- a = 1
    -- @
    ImplicitArrayForDefinedKeyError
      { _path :: [Text]
      , _existingValue :: Value
      , _tableSection :: Table
      }
  | -- | When a non-table value is already defined in a nested key, e.g.
    --
    -- @
    -- a.b = 1
    -- a.b.c.d = 2
    -- @
    NonTableInNestedKeyError
      { _path :: [Text]
      , _existingValue :: Value
      , _originalKey :: [Text]
      , _originalValue :: Value
      }
  | -- | When a non-table value is already defined in a nested implicit array, e.g.
    --
    -- @
    -- a.b = 1
    --
    -- [[a.b.c]]
    -- d = 2
    -- @
    NonTableInNestedImplicitArrayError
      { _path :: [Text]
      , _existingValue :: Value
      , _sectionKey :: [Text]
      , _tableSection :: Table
      }
  deriving (Show)

renderTOMLError :: TOMLError -> Text
renderTOMLError = \case
  ParseError s -> s
  NormalizeError DuplicateKeyError{..} ->
    Text.unlines
      [ "Could not add value to path " <> showPath _path <> ":"
      , "  Existing value: " <> renderValue _existingValue
      , "  Value to set: " <> renderValue _valueToSet
      ]
  NormalizeError DuplicateSectionError{..} -> "Found duplicate section: " <> showPath _sectionKey
  NormalizeError ImplicitArrayForDefinedKeyError{..} ->
    Text.unlines
      [ "Could not create implicit array at path " <> showPath _path <> ":"
      , "  Existing value: " <> renderValue _existingValue
      , "  Array table section: " <> renderValue (Table _tableSection)
      ]
  NormalizeError NonTableInNestedKeyError{..} ->
    Text.unlines
      [ "Found non-Table at path " <> showPath _path <> " when defining nested key " <> showPath _originalKey <> ":"
      , "  Existing value: " <> renderValue _existingValue
      , "  Original value: " <> renderValue _originalValue
      ]
  NormalizeError NonTableInNestedImplicitArrayError{..} ->
    Text.unlines
      [ "Found non-Table at path " <> showPath _path <> " when initializing implicit array at path " <> showPath _sectionKey <> ":"
      , "  Existing value: " <> renderValue _existingValue
      , "  Array table section: " <> renderValue (Table _tableSection)
      ]
  where
    showPath path = "\"" <> Text.intercalate "." path <> "\""
