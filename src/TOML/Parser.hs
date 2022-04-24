module TOML.Parser (
  parseTOML,
) where

import Data.Text (Text)

import TOML.Internal (TOMLError (..), Value (..))

parseTOML :: Text -> Either TOMLError Value
parseTOML = undefined
