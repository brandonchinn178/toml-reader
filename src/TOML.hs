module TOML (
  decode,
  decodeFile,
  Value (..),
  FromTOML (..),
) where

import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Data.Text.IO as Text

import TOML.Internal (TOMLError, Value (..))
import TOML.Parser (parseTOML)

{--- FromTOML ---}

-- TODO: do a similar thing to jordan to parse and deserialize at the same time
-- https://github.com/AnthonySuper/jordan
newtype Parser a = Parser {unParser :: Value -> Either TOMLError a}

class FromTOML a where
  fromTOML :: Parser a

instance FromTOML Value where
  fromTOML = Parser Right

{--- Decoding ---}

decode :: FromTOML a => Text -> Either TOMLError a
decode = parseTOML >=> unParser fromTOML

decodeFile :: FromTOML a => FilePath -> IO (Either TOMLError a)
decodeFile = fmap decode . Text.readFile
