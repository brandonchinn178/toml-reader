{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import System.Directory (findExecutable)
import System.Environment (getArgs, getExecutablePath)
import System.Process (callProcess)

import TOML (Value (..), decode)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Lazy as HashMap
#endif

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.ISO8601 (iso8601Show)
#else
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
#endif

main :: IO ()
main =
  getArgs >>= \case
    ["--check"] -> checkTOML
    args -> runTomlTest args

-- | Run 'toml-test -- $0 --check'
runTomlTest :: [String] -> IO ()
runTomlTest args = do
  thisExe <- getExecutablePath
  findExecutable "toml-test" >>= \case
    Nothing -> error "Could not find 'toml-test' executable"
    Just tomlTestExe ->
      callProcess tomlTestExe $
        args ++ ["-color", "always", "--", thisExe, "--check"]

-- | Parse TOML data in stdin.
checkTOML :: IO ()
checkTOML = do
  input <- Text.getContents
  output <- either (error . show) return $ decode input
  Char8.putStrLn $ Aeson.encode $ toTaggedJSON output

toTaggedJSON :: Value -> Aeson.Value
toTaggedJSON = \case
  Table o -> Aeson.Object $ toKeyMap $ toTaggedJSON <$> o
  Array vs -> Aeson.Array $ Vector.fromList $ map toTaggedJSON vs
  String x -> tagged "string" x
  Integer x -> tagged "integer" x
  Float x -> tagged "float" x
  Boolean x -> tagged "bool" x
  OffsetDateTime x -> tagged "datetime" $ iso8601Show x
  LocalDateTime x -> tagged "datetime-local" $ iso8601Show x
  LocalDate x -> tagged "date-local" $ iso8601Show x
  LocalTime x -> tagged "time-local" $ iso8601Show x
  where
    tagged :: Aeson.ToJSON a => String -> a -> Aeson.Value
    tagged ty v = Aeson.object ["type" .= ty, "value" .= v]

#if MIN_VERSION_aeson(2,0,0)
toKeyMap :: Map Text Aeson.Value -> KeyMap Aeson.Value
toKeyMap = KeyMap.fromMap . Map.mapKeys Key.fromText
#else
toKeyMap :: Map Text Aeson.Value -> Aeson.Object
toKeyMap = HashMap.fromList . Map.toList
#endif

#if !MIN_VERSION_time(1,9,0)
class ISO8601 a where
  iso8601Show :: a -> String
instance ISO8601 UTCTime where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%T%Q%Z")
instance ISO8601 LocalTime where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%T%Q")
instance ISO8601 Day where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
instance ISO8601 TimeOfDay where
  iso8601Show = formatTime defaultTimeLocale "%T%Q"
#endif
