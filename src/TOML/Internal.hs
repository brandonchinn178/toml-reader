{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TOML.Internal (
  Value (..),
  Table,
  TOMLError (..),
) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec.Error (ParseErrorBundle)

type Table = Map Text Value

data Value
  = Table Table
  | Array [Value]
  | String Text
  | Integer Integer
  | Float Double
  | Boolean Bool
  | OffsetDateTime UTCTime
  | LocalDateTime LocalTime
  | LocalDate Day
  | LocalTime TimeOfDay
  deriving (Show, Eq, Generic, NFData)

-- TODO: better names?
data TOMLError
  = ParseError (ParseErrorBundle Text Void) -- TODO: convert parse errors into our own errors
  | NormalizeError Text
  deriving (Show)
