{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TOML.Internal (
  Value (..),
  TOMLError (..),
) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import GHC.Generics (Generic)

data Value
  = Table (Map Text Value)
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

-- TODO
data TOMLError = TOMLError deriving (Show)
