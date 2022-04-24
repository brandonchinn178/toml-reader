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
import GHC.Generics (Generic)

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
  = ParseError Text
  | NormalizeError Text
  deriving (Show)
