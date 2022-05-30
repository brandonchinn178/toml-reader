module TOML.Decode (
  -- * Decoder interface
  Decoder (..),
  FromTOML (..),

  -- * Decoding functions
  decode,
  decodeWith,
  decodeWithOpts,
  decodeFile,
) where

import Data.Text (Text)
import qualified Data.Text.IO as Text

import TOML.Internal (TOMLError (..), Value (..))
import TOML.Parser (parseTOML)

{--- Decoder ---}

newtype Decoder a = Decoder
  { unDecoder :: Value -> Either TOMLError a
  }

instance Functor Decoder where
  fmap f = Decoder . fmap (fmap f) . unDecoder

instance Applicative Decoder where
  pure = Decoder . const . Right
  Decoder decodeF <*> Decoder decodeV = Decoder $ \v -> decodeF v <*> decodeV v

instance Monad Decoder where
  Decoder decodeA >>= f = Decoder $ \v -> ($ v) . unDecoder . f =<< decodeA v

{--- FromTOML ---}

class FromTOML a where
  fromTOML :: Decoder a

instance FromTOML Value where
  fromTOML = Decoder Right

{--- Decoding ---}

-- | Decode the given TOML input using the given FromTOML instance.
decode :: FromTOML a => Text -> Either TOMLError a
decode = decodeWith fromTOML

-- | Decode the given TOML input using the given Decoder.
decodeWith :: Decoder a -> Text -> Either TOMLError a
decodeWith decoder = decodeWithOpts decoder ""

decodeWithOpts :: Decoder a -> String -> Text -> Either TOMLError a
decodeWithOpts decoder filename input = parseTOML filename input >>= unDecoder decoder

-- | A helper for decoding a file at the given file path.
decodeFile :: FromTOML a => FilePath -> IO (Either TOMLError a)
decodeFile fp = decodeWithOpts fromTOML fp <$> Text.readFile fp
