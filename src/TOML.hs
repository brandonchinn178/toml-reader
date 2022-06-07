module TOML (
  -- * Decoding a TOML file
  decode,
  decodeWith,
  decodeFile,
  DecodeTOML (..),
  Decoder,

  -- ** Decoding getters
  getField,
  getFields,
  getFieldOpt,
  getFieldsOpt,
  getFieldWith,
  getFieldsWith,
  getFieldOptWith,
  getFieldsOptWith,
  getArrayOf,

  -- ** Build custom Decoder
  DecodeM,
  makeDecoder,
  runDecoder,
  invalidValue,
  typeMismatch,
  decodeFail,

  -- * TOML types
  Value (..),
  renderValue,
  Table,
  TOMLError (..),
  NormalizeError (..),
  DecodeContext,
  ContextItem (..),
  DecodeError (..),
  renderTOMLError,
) where

import TOML.Decode
import TOML.Error
import TOML.Value
