{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TOML.Decode (
  -- * Decoding functions
  decode,
  decodeWith,
  decodeWithOpts,
  decodeFile,

  -- * Decoder interface
  DecodeTOML (..),
  Decoder (..),

  -- ** Decoder getters
  getField,
  getFieldOr,
  getFields,
  getFieldOpt,
  getFieldsOpt,
  getFieldWith,
  getFieldsWith,
  getFieldOptWith,
  getFieldsOptWith,
  getArrayOf,
  getTableOf,

  -- ** Build custom Decoder
  DecodeM (..),
  makeDecoder,
  runDecoder,
  addContextItem,
  invalidValue,
  typeMismatch,
  decodeFail,
  decodeError,
) where

import Control.Applicative (Alternative (..), Const (..))
import Control.Monad (zipWithM)
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,13,0)
import qualified Control.Monad.Fail as MonadFail
#endif
import Data.Bifunctor (first)
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio)
import qualified Data.Semigroup as Semigroup
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import Data.Version (Version, parseVersion)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)

import TOML.Error (
  ContextItem (..),
  DecodeContext,
  DecodeError (..),
  TOMLError (..),
 )
import TOML.Parser (parseTOML)
import TOML.Value (Value (..))

{--- Decoder ---}

-- |
-- A @Decoder a@ represents a function for decoding a TOML value to a value of type @a@.
--
-- Generally, you'd only need to chain the @getField*@ functions together, like
--
-- @
-- decoder =
--   MyConfig
--     \<$> getField "a"
--     \<*> getField "b"
--     \<*> getField "c"
-- @
--
-- or use interfaces like 'Monad' and 'Alternative':
--
-- @
-- decoder = do
--   cfgType <- getField "type"
--   case cfgType of
--     "int" -> MyIntValue \<$> (getField "int" \<|> getField "integer")
--     "bool" -> MyBoolValue \<$> getField "bool"
--     _ -> fail $ "Invalid type: " <> cfgType
-- @
--
-- but you can also manually implement a 'Decoder' with 'makeDecoder'.
newtype Decoder a = Decoder {unDecoder :: Value -> DecodeM a}

instance Functor Decoder where
  fmap f = Decoder . (fmap . fmap) f . unDecoder
instance Applicative Decoder where
  pure v = Decoder $ \_ -> pure v
  Decoder decodeF <*> Decoder decodeV = Decoder $ \v -> decodeF v <*> decodeV v
instance Monad Decoder where
  Decoder decodeA >>= f = Decoder $ \v -> do
    a <- decodeA v
    let Decoder decodeB = f a
    decodeB v
#if !MIN_VERSION_base(4,13,0)
  fail msg = Decoder $ \_ -> decodeFail $ Text.pack msg
#endif
instance Alternative Decoder where
  empty = fail "Decoder.Alternative: empty"
  Decoder decode1 <|> Decoder decode2 = Decoder $ \v -> decode1 v <|> decode2 v
#if MIN_VERSION_base(4,13,0)
instance MonadFail Decoder where
  fail msg = Decoder $ \_ -> decodeFail $ Text.pack msg
#elif MIN_VERSION_base(4,9,0)
instance MonadFail.MonadFail Decoder where
  fail msg = Decoder $ \_ -> decodeFail $ Text.pack msg
#endif

-- | Manually implement a 'Decoder' with the given function.
makeDecoder :: (Value -> DecodeM a) -> Decoder a
makeDecoder = Decoder

decoderToEither :: Decoder a -> Value -> DecodeContext -> Either (DecodeContext, DecodeError) a
decoderToEither decoder v ctx = unDecodeM (unDecoder decoder v) ctx

-- | The underlying decoding monad that either returns a value of type @a@ or returns an error.
newtype DecodeM a = DecodeM {unDecodeM :: DecodeContext -> Either (DecodeContext, DecodeError) a}

instance Functor DecodeM where
  fmap f = DecodeM . (fmap . fmap) f . unDecodeM
instance Applicative DecodeM where
  pure v = DecodeM $ \_ -> pure v
  DecodeM decodeF <*> DecodeM decodeV = DecodeM $ \ctx -> decodeF ctx <*> decodeV ctx
instance Monad DecodeM where
  DecodeM decodeA >>= f = DecodeM $ \ctx -> do
    a <- decodeA ctx
    let DecodeM decodeB = f a
    decodeB ctx
#if !MIN_VERSION_base(4,13,0)
  fail = decodeFail . Text.pack
#endif
instance Alternative DecodeM where
  empty = decodeFail "DecodeM.Alternative: empty"
  DecodeM decode1 <|> DecodeM decode2 = DecodeM $ \ctx ->
    case decode1 ctx of
      Left _ -> decode2 ctx
      Right x -> Right x
#if MIN_VERSION_base(4,13,0)
instance MonadFail DecodeM where
  fail = decodeFail . Text.pack
#elif MIN_VERSION_base(4,9,0)
instance MonadFail.MonadFail DecodeM where
  fail = decodeFail . Text.pack
#endif

-- |
-- Run a 'Decoder' with the given 'Value'.
--
-- @
-- makeDecoder $ \\v -> do
--   a <- runDecoder decoder1 v
--   b <- runDecoder decoder2 v
--   return (a, b)
-- @
--
-- Satisfies
--
-- @
-- makeDecoder . runDecoder === id
-- runDecoder . makeDecoder === id
-- @
runDecoder :: Decoder a -> Value -> DecodeM a
runDecoder decoder v = DecodeM (decoderToEither decoder v)

-- |
-- Throw an error indicating that the given 'Value' is invalid.
--
-- @
-- makeDecoder $ \\v ->
--   case v of
--     Integer 42 -> invalidValue "We don't like this number" v
--     _ -> runDecoder tomlDecoder v
--
-- -- or alternatively,
-- tomlDecoder >>= \case
--   42 -> makeDecoder $ invalidValue "We don't like this number"
--   v -> pure v
-- @
invalidValue :: Text -> Value -> DecodeM a
invalidValue msg v = decodeError $ InvalidValue msg v

-- |
-- Throw an error indicating that the given 'Value' isn't the correct type of value.
--
-- @
-- makeDecoder $ \\v ->
--   case v of
--     String s -> ...
--     _ -> typeMismatch v
-- @
typeMismatch :: Value -> DecodeM a
typeMismatch v = decodeError $ TypeMismatch v

-- | Throw a generic failure message.
decodeFail :: Text -> DecodeM a
decodeFail msg = decodeError $ OtherDecodeError msg

-- | Throw the given 'DecodeError'.
decodeError :: DecodeError -> DecodeM a
decodeError e = DecodeM $ \ctx -> Left (ctx, e)

addContextItem :: ContextItem -> DecodeM a -> DecodeM a
addContextItem p m = DecodeM $ \ctx -> unDecodeM m (ctx <> [p])

{--- Decoding ---}

-- | Decode the given TOML input.
decode :: (DecodeTOML a) => Text -> Either TOMLError a
decode = decodeWith tomlDecoder

-- | Decode the given TOML input using the given 'Decoder'.
decodeWith :: Decoder a -> Text -> Either TOMLError a
decodeWith decoder = decodeWithOpts decoder ""

decodeWithOpts :: Decoder a -> String -> Text -> Either TOMLError a
decodeWithOpts decoder filename input = do
  v <- parseTOML filename input
  first (uncurry DecodeError) $ decoderToEither decoder v []

-- | Decode a TOML file at the given file path.
decodeFile :: (DecodeTOML a) => FilePath -> IO (Either TOMLError a)
decodeFile fp = decodeWithOpts tomlDecoder fp <$> Text.readFile fp

{--- Decoder helpers ---}

-- |
-- Decode a field in a TOML Value.
-- Equivalent to 'getFields' with a single-element list.
--
-- @
-- a = 1
-- b = 'asdf'
-- @
--
-- @
-- -- MyConfig 1 "asdf"
-- MyConfig \<$> getField "a" \<*> getField "b"
-- @
getField :: (DecodeTOML a) => Text -> Decoder a
getField = getFieldWith tomlDecoder

-- |
-- Decode a field in a TOML Value or succeed with a default value when the field is missing.
--
-- @
-- a = 1
-- # b is missing
-- @
--
-- @
-- -- MyConfig 1 "asdf"
-- MyConfig \<$> getFieldOr 42 "a" \<*> getFieldOr "asdf" "b"
-- @
getFieldOr :: (DecodeTOML a) => a -> Text -> Decoder a
getFieldOr def key = fromMaybe def <$> getFieldOpt key

-- | Same as 'getField', except with the given 'Decoder'.
getFieldWith :: Decoder a -> Text -> Decoder a
getFieldWith decoder key = getFieldsWith decoder [key]

-- |
-- Decode a field in a TOML Value, or Nothing if the field doesn't exist.
-- Equivalent to 'getFieldsOpt' with a single-element list.
--
-- @
-- a = 1
-- @
--
-- @
-- -- MyConfig (Just 1) Nothing
-- MyConfig \<$> getFieldOpt "a" \<*> getFieldOpt "b"
-- @
getFieldOpt :: (DecodeTOML a) => Text -> Decoder (Maybe a)
getFieldOpt = getFieldOptWith tomlDecoder

-- | Same as 'getFieldOpt', except with the given 'Decoder'.
getFieldOptWith :: Decoder a -> Text -> Decoder (Maybe a)
getFieldOptWith decoder key = getFieldsOptWith decoder [key]

-- |
-- Decode a nested field in a TOML Value.
--
-- @
-- a.b = 1
-- @
--
-- @
-- -- MyConfig 1
-- MyConfig \<$> getFields ["a", "b"]
-- @
getFields :: (DecodeTOML a) => [Text] -> Decoder a
getFields = getFieldsWith tomlDecoder

-- | Same as 'getFields', except with the given 'Decoder'.
getFieldsWith :: Decoder a -> [Text] -> Decoder a
getFieldsWith decoder = makeDecoder . go
  where
    go [] v = runDecoder decoder v
    go (k : ks) v =
      case v of
        Table o ->
          addContextItem (Key k) $
            case Map.lookup k o of
              Just v' -> go ks v'
              Nothing -> decodeError MissingField
        _ -> typeMismatch v

-- |
-- Decode a nested field in a TOML Value, or 'Nothing' if any of the fields don't exist.
--
-- @
-- a.b = 1
-- @
--
-- @
-- -- MyConfig (Just 1) Nothing Nothing
-- MyConfig
--   \<$> getFieldsOpt ["a", "b"]
--   \<*> getFieldsOpt ["a", "c"]
--   \<*> getFieldsOpt ["b", "c"]
-- @
getFieldsOpt :: (DecodeTOML a) => [Text] -> Decoder (Maybe a)
getFieldsOpt = getFieldsOptWith tomlDecoder

-- | Same as 'getFieldsOpt', except with the given 'Decoder'.
getFieldsOptWith :: Decoder a -> [Text] -> Decoder (Maybe a)
getFieldsOptWith decoder keys =
  makeDecoder $ \v ->
    DecodeM $ \ctx ->
      case (`unDecodeM` ctx) . (`runDecoder` v) $ getFieldsWith decoder keys of
        Left (_, MissingField) -> Right Nothing
        Left (ctx', e) -> Left (ctx', e)
        Right x -> Right $ Just x

-- |
-- Decode a list of values using the given 'Decoder'.
--
-- @
-- [[a]]
-- b = 1
--
-- [[a]]
-- b = 2
-- @
--
-- @
-- -- MyConfig [1, 2]
-- MyConfig
--   \<$> getFieldWith (getArrayOf (getField "b")) "a"
-- @
getArrayOf :: Decoder a -> Decoder [a]
getArrayOf decoder =
  makeDecoder $ \case
    Array vs -> zipWithM (\i -> addContextItem (Index i) . runDecoder decoder) [0 ..] vs
    v -> typeMismatch v

-- |
-- Decode a table of values using a given 'Decoder', returning the
-- results as a 'Text'-indexed 'Map'.
getTableOf :: Decoder a -> Decoder (Map Text a)
getTableOf decoder =
  makeDecoder $ \case
    Table t -> Map.traverseWithKey (\k -> addContextItem (Key k) . runDecoder decoder) t
    v -> typeMismatch v

{--- DecodeTOML ---}

-- |
-- A type class containing the default 'Decoder' for the given type.
--
-- See the docs for 'Decoder' for examples.
class DecodeTOML a where
  tomlDecoder :: Decoder a

instance DecodeTOML Value where
  tomlDecoder = Decoder pure

instance DecodeTOML Void where
  tomlDecoder = makeDecoder typeMismatch
instance DecodeTOML Bool where
  tomlDecoder =
    makeDecoder $ \case
      Boolean x -> pure x
      v -> typeMismatch v

instance DecodeTOML Integer where
  tomlDecoder =
    makeDecoder $ \case
      Integer x -> pure x
      v -> typeMismatch v

tomlDecoderInt :: forall a. (Num a) => Decoder a
tomlDecoderInt = fromInteger <$> tomlDecoder

tomlDecoderBoundedInt :: forall a. (Integral a, Bounded a) => Decoder a
tomlDecoderBoundedInt =
  tomlDecoder >>= \case
    x
      | x < toInteger (minBound @a) -> makeDecoder $ invalidValue "Underflow"
      | x > toInteger (maxBound @a) -> makeDecoder $ invalidValue "Overflow"
      | otherwise -> pure $ fromInteger x

instance DecodeTOML Int where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Int8 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Int16 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Int32 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Int64 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Word where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Word8 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Word16 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Word32 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Word64 where
  tomlDecoder = tomlDecoderBoundedInt
instance DecodeTOML Natural where
  tomlDecoder =
    tomlDecoder >>= \case
      x
        | x >= 0 -> pure $ fromInteger x
        | otherwise -> makeDecoder $ invalidValue "Got negative number"

instance DecodeTOML Double where
  tomlDecoder =
    makeDecoder $ \case
      Float x -> pure x
      v -> typeMismatch v

tomlDecoderFrac :: (Fractional a) => Decoder a
tomlDecoderFrac = realToFrac <$> tomlDecoder @Double

instance DecodeTOML Float where
  tomlDecoder = tomlDecoderFrac
instance (Integral a) => DecodeTOML (Ratio a) where
  tomlDecoder = tomlDecoderFrac
instance (HasResolution a) => DecodeTOML (Fixed a) where
  tomlDecoder = tomlDecoderFrac

instance DecodeTOML Char where
  tomlDecoder =
    tomlDecoder >>= \case
      s
        | Text.length s == 1 -> pure $ Text.head s
        | otherwise -> makeDecoder $ invalidValue "Expected single character string"
instance {-# OVERLAPPING #-} DecodeTOML String where
  tomlDecoder = Text.unpack <$> tomlDecoder
instance DecodeTOML Text where
  tomlDecoder =
    makeDecoder $ \case
      String s -> pure s
      v -> typeMismatch v
instance DecodeTOML Lazy.Text where
  tomlDecoder = Text.Lazy.fromStrict <$> tomlDecoder

instance DecodeTOML Time.ZonedTime where
  tomlDecoder =
    makeDecoder $ \case
      OffsetDateTime (lt, tz) -> pure $ Time.ZonedTime lt tz
      v -> typeMismatch v
instance DecodeTOML Time.UTCTime where
  tomlDecoder = Time.zonedTimeToUTC <$> tomlDecoder
instance DecodeTOML Time.SystemTime where
  tomlDecoder = Time.utcToSystemTime . Time.zonedTimeToUTC <$> tomlDecoder
instance DecodeTOML Time.LocalTime where
  tomlDecoder =
    makeDecoder $ \case
      LocalDateTime dt -> pure dt
      v -> typeMismatch v
instance DecodeTOML Time.Day where
  tomlDecoder =
    makeDecoder $ \case
      LocalDate d -> pure d
      v -> typeMismatch v
instance DecodeTOML Time.TimeOfDay where
  tomlDecoder =
    makeDecoder $ \case
      LocalTime t -> pure t
      v -> typeMismatch v
#if MIN_VERSION_time(1,9,0)
instance DecodeTOML Time.DayOfWeek where
  tomlDecoder = toDayOfWeek . Text.toLower =<< tomlDecoder
    where
      toDayOfWeek = \case
        "monday" -> pure Time.Monday
        "tuesday" -> pure Time.Tuesday
        "wednesday" -> pure Time.Wednesday
        "thursday" -> pure Time.Thursday
        "friday" -> pure Time.Friday
        "saturday" -> pure Time.Saturday
        "sunday" -> pure Time.Sunday
        _ -> makeDecoder $ invalidValue "Invalid day of week"
#endif

instance DecodeTOML Time.DiffTime where
  tomlDecoder = tomlDecoderInt <|> tomlDecoderFrac
instance DecodeTOML Time.NominalDiffTime where
  tomlDecoder = tomlDecoderInt <|> tomlDecoderFrac
#if MIN_VERSION_time(1,9,0)
instance DecodeTOML Time.CalendarDiffTime where
  tomlDecoder =
    Time.CalendarDiffTime
      <$> getField "months"
      <*> getField "time"
instance DecodeTOML Time.CalendarDiffDays where
  tomlDecoder =
    Time.CalendarDiffDays
      <$> getField "months"
      <*> getField "days"
#endif

instance DecodeTOML Version where
  tomlDecoder = go . readP_to_S parseVersion =<< tomlDecoder
    where
      go ((v, []) : _) = pure v
      go (_ : vs) = go vs
      go [] = makeDecoder $ invalidValue "Invalid Version"
instance DecodeTOML Ordering where
  tomlDecoder =
    tomlDecoder @Text >>= \case
      "LT" -> pure LT
      "EQ" -> pure EQ
      "GT" -> pure GT
      _ -> makeDecoder $ invalidValue "Invalid Ordering"

instance (DecodeTOML a) => DecodeTOML (Identity a) where
  tomlDecoder = Identity <$> tomlDecoder
instance DecodeTOML (Proxy a) where
  tomlDecoder = pure Proxy
instance (DecodeTOML a) => DecodeTOML (Const a b) where
  tomlDecoder = Const <$> tomlDecoder

-- |
-- Since TOML doesn't support literal NULLs, this will only ever return 'Just'.
-- To get the absence of a field, use 'getFieldOpt' or one of its variants.
instance (DecodeTOML a) => DecodeTOML (Maybe a) where
  tomlDecoder = Just <$> tomlDecoder

instance (DecodeTOML a, DecodeTOML b) => DecodeTOML (Either a b) where
  tomlDecoder = (Right <$> tomlDecoder) <|> (Left <$> tomlDecoder)

instance (DecodeTOML a) => DecodeTOML (Monoid.First a) where
  tomlDecoder = Monoid.First <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Monoid.Last a) where
  tomlDecoder = Monoid.Last <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Semigroup.First a) where
  tomlDecoder = Semigroup.First <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Semigroup.Last a) where
  tomlDecoder = Semigroup.Last <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Semigroup.Max a) where
  tomlDecoder = Semigroup.Max <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Semigroup.Min a) where
  tomlDecoder = Semigroup.Min <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Monoid.Dual a) where
  tomlDecoder = Monoid.Dual <$> tomlDecoder

instance (DecodeTOML a) => DecodeTOML [a] where
  tomlDecoder = getArrayOf tomlDecoder
instance (IsString k, Ord k, DecodeTOML v) => DecodeTOML (Map k v) where
  tomlDecoder = Map.mapKeys (fromString . Text.unpack) <$> getTableOf tomlDecoder
instance (DecodeTOML a) => DecodeTOML (NonEmpty a) where
  tomlDecoder = maybe raiseEmpty pure . NonEmpty.nonEmpty =<< tomlDecoder
    where
      raiseEmpty = makeDecoder $ invalidValue "Got empty list"
instance DecodeTOML IntSet where
  tomlDecoder = IntSet.fromList <$> tomlDecoder
instance (DecodeTOML a, Ord a) => DecodeTOML (Set a) where
  tomlDecoder = Set.fromList <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (IntMap a) where
  tomlDecoder = IntMap.fromList <$> tomlDecoder
instance (DecodeTOML a) => DecodeTOML (Seq a) where
  tomlDecoder = Seq.fromList <$> tomlDecoder

tomlDecoderTuple :: ([Value] -> Maybe (DecodeM a)) -> Decoder a
tomlDecoderTuple f =
  makeDecoder $ \case
    Array vs | Just decodeM <- f vs -> decodeM
    v -> typeMismatch v
decodeElem :: (DecodeTOML a) => Int -> Value -> DecodeM a
decodeElem i v = addContextItem (Index i) (runDecoder tomlDecoder v)
instance DecodeTOML () where
  tomlDecoder = tomlDecoderTuple $ \case
    [] -> Just $ pure ()
    _ -> Nothing
instance (DecodeTOML a, DecodeTOML b) => DecodeTOML (a, b) where
  tomlDecoder = tomlDecoderTuple $ \case
    [a, b] ->
      Just $
        (,)
          <$> decodeElem 0 a
          <*> decodeElem 1 b
    _ -> Nothing
instance (DecodeTOML a, DecodeTOML b, DecodeTOML c) => DecodeTOML (a, b, c) where
  tomlDecoder = tomlDecoderTuple $ \case
    [a, b, c] ->
      Just $
        (,,)
          <$> decodeElem 0 a
          <*> decodeElem 1 b
          <*> decodeElem 2 c
    _ -> Nothing
instance (DecodeTOML a, DecodeTOML b, DecodeTOML c, DecodeTOML d) => DecodeTOML (a, b, c, d) where
  tomlDecoder = tomlDecoderTuple $ \case
    [a, b, c, d] ->
      Just $
        (,,,)
          <$> decodeElem 0 a
          <*> decodeElem 1 b
          <*> decodeElem 2 c
          <*> decodeElem 3 d
    _ -> Nothing
