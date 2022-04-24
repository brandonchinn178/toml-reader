module TOML (
  decode,
  -- TODO
) where

import Data.Text (Text)

decode :: FromTOML a => Text -> Either TOMLError a
decode = undefined

-- TODO
data TOMLError
class FromTOML a
