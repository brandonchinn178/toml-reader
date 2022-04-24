module TOML (
  decode,
  Value (..),
) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)

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
  deriving (Show, Eq)

decode :: FromTOML a => Text -> Either TOMLError a
decode = undefined

-- TODO
data TOMLError = TOMLError deriving (Show)
class FromTOML a
instance FromTOML Value
