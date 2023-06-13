{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (ZonedTime (..))
import qualified Data.Vector as Vector
import System.Directory (findExecutable)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Process (callProcess)

import TOML (Value (..), renderTOMLError)
import TOML.Parser (parseTOML)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.HashMap.Lazy ()
#else
import qualified Data.HashMap.Lazy as HashMap
#endif

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.ISO8601 (iso8601Show)
#else
import Data.Time (Day, LocalTime, TimeOfDay)
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
    Nothing ->
      -- don't error, so that Hackage's test run doesn't fail
      putStrLn "WARNING: toml-test not installed. Skipping test suite..."
    Just tomlTestExe ->
      callProcess tomlTestExe $
        args ++ ["-color", "always", "--", thisExe, "--check"]

-- | Parse TOML data in stdin.
checkTOML :: IO ()
checkTOML = do
  input <- Text.getContents
  output <- either handleError return $ parseTOML "<stdin>" input
  Char8.putStrLn $ Aeson.encode $ toTaggedJSON output
  where
    handleError e = do
      Text.hPutStrLn stderr $ renderTOMLError e
      exitFailure

toTaggedJSON :: Value -> Aeson.Value
toTaggedJSON = \case
  Table o -> Aeson.Object $ toKeyMap $ toTaggedJSON <$> o
  Array vs -> Aeson.Array $ Vector.fromList $ map toTaggedJSON vs
  String x -> tagged "string" (Text.unpack x)
  Integer x -> tagged "integer" (show x)
  Float x -> tagged "float" (showFloat x)
  Boolean x -> tagged "bool" $ if x then "true" else "false"
  OffsetDateTime (lt, tz) -> tagged "datetime" $ iso8601Show (ZonedTime lt tz)
  LocalDateTime x -> tagged "datetime-local" $ iso8601Show x
  LocalDate x -> tagged "date-local" $ iso8601Show x
  LocalTime x -> tagged "time-local" $ iso8601Show x
  where
    tagged :: String -> String -> Aeson.Value
    tagged ty v = Aeson.object ["type" .= ty, "value" .= v]

    showFloat x
      | isNaN x = "nan"
      | isInfinite x = if x < 0 then "-inf" else "inf"
      | otherwise = show x

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
instance ISO8601 ZonedTime where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%T%Q%Z")
instance ISO8601 LocalTime where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%T%Q")
instance ISO8601 Day where
  iso8601Show = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
instance ISO8601 TimeOfDay where
  iso8601Show = formatTime defaultTimeLocale "%T%Q"
#endif
