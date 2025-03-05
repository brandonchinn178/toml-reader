{-# LANGUAGE OverloadedStrings #-}

module TOML.ErrorSpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Skeletest
import Skeletest.Predicate qualified as P

import TOML.Error (
  ContextItem (..),
  DecodeError (..),
  NormalizeError (..),
  TOMLError (..),
  renderTOMLError,
 )
import TOML.Value (Value (..))

spec :: Spec
spec = do
  describe "renderTOMLError" $ do
    forM_ allErrors $ \(label, e) ->
      it ("renders " <> label) $ do
        renderTOMLError e `shouldSatisfy` P.matchesSnapshot

    it "renders context items correctly" $ do
      let msg = renderTOMLError (DecodeError [Key "a", Index 1, Key "b"] MissingField)
      msg `shouldSatisfy` P.hasPrefix "Decode error at '.a[1].b':"
  where
    allErrors =
      [ ("ParseError", ParseError "megaparsec error")
      ,
        ( "NormalizeError.DuplicateKeyError"
        , NormalizeError
            DuplicateKeyError
              { _path = fullPath
              , _existingValue = value1
              , _valueToSet = value2
              }
        )
      ,
        ( "NormalizeError.DuplicateSectionError"
        , NormalizeError
            DuplicateSectionError
              { _sectionKey = fullPath
              }
        )
      ,
        ( "NormalizeError.ExtendTableError"
        , NormalizeError
            ExtendTableError
              { _path = subPath
              , _originalKey = fullPath
              }
        )
      ,
        ( "NormalizeError.ExtendTableInInlineArrayError"
        , NormalizeError
            ExtendTableInInlineArrayError
              { _path = subPath
              , _originalKey = fullPath
              }
        )
      ,
        ( "NormalizeError.ImplicitArrayForDefinedKeyError"
        , NormalizeError
            ImplicitArrayForDefinedKeyError
              { _path = fullPath
              , _existingValue = Array [value1]
              , _tableSection = Map.fromList [("a", value2)]
              }
        )
      ,
        ( "NormalizeError.NonTableInNestedKeyError"
        , NormalizeError
            NonTableInNestedKeyError
              { _path = subPath
              , _existingValue = value1
              , _originalKey = fullPath
              , _originalValue = value2
              }
        )
      ,
        ( "NormalizeError.NonTableInNestedImplicitArrayError"
        , NormalizeError
            NonTableInNestedImplicitArrayError
              { _path = subPath
              , _existingValue = value1
              , _sectionKey = fullPath
              , _tableSection = Map.fromList [("a", value2)]
              }
        )
      , ("DecodeError.MissingField", DecodeError ctx MissingField)
      , ("DecodeError.InvalidValue", DecodeError ctx $ InvalidValue "bad value" value1)
      , ("DecodeError.TypeMismatch", DecodeError ctx $ TypeMismatch value1)
      , ("DecodeError.OtherDecodeError", DecodeError ctx $ OtherDecodeError "decode failure")
      ]
    fullPath = NonEmpty.fromList ["a", "b"]
    subPath = NonEmpty.fromList ["a"]
    value1 = Boolean True
    value2 = Integer 1
    ctx = [Key "a", Key "b"]
