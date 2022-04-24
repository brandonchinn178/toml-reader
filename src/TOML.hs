module TOML (
  decode,
  decodeFile,
  Value (..),
  Table,
  FromTOML (..),
  TOMLError (..),
) where

import Data.Text (Text)
import qualified Data.Text.IO as Text

import TOML.Internal (TOMLError (..), Table, Value (..))
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
decode = decodeWithFilename ""

decodeFile :: FromTOML a => FilePath -> IO (Either TOMLError a)
decodeFile fp = decodeWithFilename fp <$> Text.readFile fp

decodeWithFilename :: FromTOML a => String -> Text -> Either TOMLError a
decodeWithFilename filename input = parseTOML filename input >>= unParser fromTOML
