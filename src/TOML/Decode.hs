{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  getFields,
  getFieldOpt,
  getFieldsOpt,
  getFieldWith,
  getFieldsWith,
  getFieldOptWith,
  getFieldsOptWith,
  getArrayOf,

  -- ** Build custom Decoder
  DecodeM (..),
  makeDecoder,
  runDecoder,
  invalidValue,
  typeMismatch,
  decodeFail,
  decodeError,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (zipWithM)
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,13,0)
import qualified Control.Monad.Fail as MonadFail
#endif
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import TOML.Internal (
  ContextItem (..),
  DecodeContext,
  DecodeError (..),
  TOMLError (..),
  Value (..),
 )
import TOML.Parser (parseTOML)

{--- Decoder ---}

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

makeDecoder :: (Value -> DecodeM a) -> Decoder a
makeDecoder = Decoder

decoderToEither :: Decoder a -> Value -> DecodeContext -> Either (DecodeContext, DecodeError) a
decoderToEither decoder v ctx = unDecodeM (unDecoder decoder v) ctx

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

runDecoder :: Decoder a -> Value -> DecodeM a
runDecoder decoder v = DecodeM (decoderToEither decoder v)

invalidValue :: Text -> Value -> DecodeM a
invalidValue msg v = decodeError $ InvalidValue msg v

typeMismatch :: Value -> DecodeM a
typeMismatch v = decodeError $ TypeMismatch v

decodeFail :: Text -> DecodeM a
decodeFail msg = decodeError $ OtherDecodeError msg

decodeError :: DecodeError -> DecodeM a
decodeError e = DecodeM $ \ctx -> Left (ctx, e)

addContextItem :: ContextItem -> DecodeM a -> DecodeM a
addContextItem p m = DecodeM $ \ctx -> unDecodeM m (ctx <> [p])

{--- Decoding ---}

-- | Decode the given TOML input using the given DecodeTOML instance.
decode :: DecodeTOML a => Text -> Either TOMLError a
decode = decodeWith tomlDecoder

-- | Decode the given TOML input using the given Decoder.
decodeWith :: Decoder a -> Text -> Either TOMLError a
decodeWith decoder = decodeWithOpts decoder ""

decodeWithOpts :: Decoder a -> String -> Text -> Either TOMLError a
decodeWithOpts decoder filename input = do
  v <- parseTOML filename input
  first (uncurry DecodeError) $ decoderToEither decoder v []

-- | A helper for decoding a file at the given file path.
decodeFile :: DecodeTOML a => FilePath -> IO (Either TOMLError a)
decodeFile fp = decodeWithOpts tomlDecoder fp <$> Text.readFile fp

{--- Decoder helpers ---}

{- |
Decode a field in a TOML Value.
Equivalent to 'getFields' with a single-element list.
-}
getField :: DecodeTOML a => Text -> Decoder a
getField = getFieldWith tomlDecoder

-- | Same as 'getField', except with the provided 'Decoder'.
getFieldWith :: Decoder a -> Text -> Decoder a
getFieldWith decoder key = getFieldsWith decoder [key]

{- |
Decode a field in a TOML Value, or Nothing if the field doesn't exist.
Equivalent to 'getFieldsOpt' with a single-element list.
-}
getFieldOpt :: DecodeTOML a => Text -> Decoder (Maybe a)
getFieldOpt = getFieldOptWith tomlDecoder

-- | Same as 'getFieldOpt', except with the provided 'Decoder'.
getFieldOptWith :: Decoder a -> Text -> Decoder (Maybe a)
getFieldOptWith decoder key = getFieldsOptWith decoder [key]

-- | Decode a nested field in a TOML Value.
getFields :: DecodeTOML a => [Text] -> Decoder a
getFields = getFieldsWith tomlDecoder

-- | Same as 'getFields', except with the given 'Decoder'.
getFieldsWith :: Decoder a -> [Text] -> Decoder a
getFieldsWith decoder = makeDecoder . go
  where
    go [] v = runDecoder decoder v
    go (k : ks) v =
      addContextItem (Key k) $
        case v of
          Table o ->
            case Map.lookup k o of
              Just v' -> go ks v'
              Nothing -> decodeError MissingField
          _ -> typeMismatch v

-- | Decode a nested field in a TOML Value, or Nothing if any of the fields don't exist.
getFieldsOpt :: DecodeTOML a => [Text] -> Decoder (Maybe a)
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

getArrayOf :: Decoder a -> Decoder [a]
getArrayOf decoder =
  makeDecoder $ \case
    Array vs -> zipWithM (\i -> addContextItem (Index i) . runDecoder decoder) [0 ..] vs
    v -> typeMismatch v

{--- DecodeTOML ---}

class DecodeTOML a where
  tomlDecoder :: Decoder a

instance DecodeTOML Value where
  tomlDecoder = Decoder pure

instance DecodeTOML Integer where
  tomlDecoder =
    makeDecoder $ \case
      Integer x -> pure x
      _ -> typeMismatch
tomlDecoderInt :: forall a. (Integral a, Bounded a) => Decoder a
tomlDecoderInt =
  tomlDecoder >>= \case
    x
      | x < toInteger (minBound @a) -> invalidValue "Underflow"
      | x > toInteger (maxBound @a) -> invalidValue "Overflow"
      | otherwise -> pure $ fromInteger x
instance DecodeTOML Int where
  tomlDecoder = tomlDecoderInt
