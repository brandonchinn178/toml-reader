{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Error (
  TOMLError (..),
  NormalizeError (..),
  DecodeContext,
  ContextItem (..),
  DecodeError (..),
  renderTOMLError,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text

import TOML.Value (Table, Value (..), renderValue)

data TOMLError
  = ParseError Text
  | NormalizeError NormalizeError
  | DecodeError DecodeContext DecodeError
  deriving (Show, Eq)

data NormalizeError
  = -- | When a key is defined twice, e.g.
    --
    -- @
    -- name = 'First'
    -- name = 'Second'
    -- @
    DuplicateKeyError
      { _path :: NonEmpty Text
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
      { _sectionKey :: NonEmpty Text
      }
  | -- | When a key attempts to extend an invalid table
    --
    -- @
    -- a = {}
    -- [a.b]
    --
    -- b = {}
    -- b.a = 1
    --
    -- c.x.x = 1
    -- [c.a]
    -- @
    ExtendTableError
      { _path :: NonEmpty Text
      , _originalKey :: NonEmpty Text
      }
  | -- | When a section attempts to extend a table within an inline array
    --
    -- @
    -- a = [{ b = 1 }]
    -- [a.c]
    -- @
    ExtendTableInInlineArrayError
      { _path :: NonEmpty Text
      , _originalKey :: NonEmpty Text
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
      { _path :: NonEmpty Text
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
      { _path :: NonEmpty Text
      , _existingValue :: Value
      , _originalKey :: NonEmpty Text
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
      { _path :: NonEmpty Text
      , _existingValue :: Value
      , _sectionKey :: NonEmpty Text
      , _tableSection :: Table
      }
  deriving (Show, Eq)

type DecodeContext = [ContextItem]

data ContextItem = Key Text | Index Int
  deriving (Show, Eq)

data DecodeError
  = MissingField
  | InvalidValue Text Value
  | TypeMismatch Value
  | OtherDecodeError Text
  deriving (Show, Eq)

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
  NormalizeError ExtendTableError{..} ->
    Text.unlines
      [ "Invalid table key: " <> showPath _originalKey
      , "  Table already statically defined at " <> showPath _path
      ]
  NormalizeError ExtendTableInInlineArrayError{..} ->
    Text.unlines
      [ "Invalid table key: " <> showPath _originalKey
      , "  Table defined in inline array at " <> showPath _path
      ]
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
  DecodeError ctx e -> "Decode error at '" <> renderDecodeContext ctx <> "': " <> renderDecodeError e
  where
    showPath path = "\"" <> Text.intercalate "." (NonEmpty.toList path) <> "\""

    renderDecodeError = \case
      MissingField -> "Field does not exist"
      InvalidValue msg v -> "Invalid value: " <> msg <> ": " <> renderValue v
      TypeMismatch v -> "Type mismatch, got: " <> renderValue v
      OtherDecodeError msg -> msg

    renderDecodeContext = Text.concat . map renderContextItem
    renderContextItem = \case
      Key k -> "." <> k
      Index i -> "[" <> Text.pack (show i) <> "]"
