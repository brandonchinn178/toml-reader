module TOML (
  -- * Decoding a TOML file
  decode,
  decodeFile,
  FromTOML (..),
  Decoder,

  -- * TOML types
  Value (..),
  Table,
  TOMLError (..),
) where

import TOML.Decode
import TOML.Internal
