{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TOML.DecodeSpec (spec) where

import Data.Int (Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Numeric.Natural (Natural)
import Skeletest
import Skeletest.Predicate qualified as P

import TOML.Decode (
  DecodeTOML,
  Decoder,
  decodeWith,
  getArrayOf,
  getField,
  getFieldOpt,
  getFieldOptWith,
  getFieldOr,
  getFieldWith,
  getFields,
  getFieldsOpt,
  getFieldsOptWith,
  getFieldsWith,
  tomlDecoder,
 )
import TOML.Error (
  ContextItem (..),
  DecodeError (..),
  TOMLError (..),
 )
import TOML.Value (Value (..))

-- | A test decoder
succDecoder :: (Enum a, DecodeTOML a) => Decoder a
succDecoder = succ <$> tomlDecoder

spec :: Spec
spec = do
  describe "getField" $ do
    it "decodes field" $
      decodeWith (getField @Int "a") "a = 1" `shouldBe` Right 1

    it "errors if field does not exist" $
      decodeWith (getField @Int "a") "" `shouldBe` Left (DecodeError [Key "a"] MissingField)

    it "errors if field is the wrong type" $
      decodeWith (getField @Int "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

  describe "getFieldOr" $ do
    it "decodes field" $
      decodeWith (getFieldOr @Int 42 "a") "a = 1" `shouldBe` Right 1

    it "returns default if field does not exist" $
      decodeWith (getFieldOr @Int 42 "a") "" `shouldBe` Right 42

    it "errors if field is the wrong type" $
      decodeWith (getFieldOr @Int 42 "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

  describe "getFieldOpt" $ do
    it "decodes field" $
      decodeWith (getFieldOpt @Int "a") "a = 1" `shouldBe` Right (Just 1)

    it "returns Nothing if field does not exist" $
      decodeWith (getFieldOpt @Int "a") "" `shouldBe` Right Nothing

    it "errors if field is the wrong type" $
      decodeWith (getFieldOpt @Int "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

  describe "getFieldWith" $ do
    it "decodes field" $
      decodeWith (getFieldWith @Int succDecoder "a") "a = 1" `shouldBe` Right 2

    it "errors if field does not exist" $
      decodeWith (getFieldWith @Int succDecoder "a") "" `shouldBe` Left (DecodeError [Key "a"] MissingField)

    it "errors if field is the wrong type" $
      decodeWith (getFieldWith @Int succDecoder "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

  describe "getFieldOptWith" $ do
    it "decodes field" $
      decodeWith (getFieldOptWith @Int succDecoder "a") "a = 1" `shouldBe` Right (Just 2)

    it "returns Nothing if field does not exist" $
      decodeWith (getFieldOptWith @Int succDecoder "a") "" `shouldBe` Right Nothing

    it "errors if field is the wrong type" $
      decodeWith (getFieldOptWith @Int succDecoder "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

  describe "getFields" $ do
    it "decodes field" $
      decodeWith (getFields @Int ["a", "b"]) "a = { b = 1 }" `shouldBe` Right 1

    it "errors if field does not exist" $
      decodeWith (getFields @Int ["a", "b"]) "a = {}" `shouldBe` Left (DecodeError [Key "a", Key "b"] MissingField)

    it "errors if intermediate field does not exist" $
      decodeWith (getFields @Int ["a", "b", "c"]) "a = {}" `shouldBe` Left (DecodeError [Key "a", Key "b"] MissingField)

    it "errors if field is the wrong type" $
      decodeWith (getFields @Int ["a", "b"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

    it "errors if intermediate field is the wrong type" $
      decodeWith (getFields @Int ["a", "b", "c"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

  describe "getFieldsOpt" $ do
    it "decodes field" $
      decodeWith (getFieldsOpt @Int ["a", "b"]) "a = { b = 1 }" `shouldBe` Right (Just 1)

    it "returns Nothing if field does not exist" $
      decodeWith (getFieldsOpt @Int ["a", "b"]) "a = {}" `shouldBe` Right Nothing

    it "returns Nothing if intermediate field does not exist" $
      decodeWith (getFieldsOpt @Int ["a", "b", "c"]) "a = {}" `shouldBe` Right Nothing

    it "errors if field is the wrong type" $
      decodeWith (getFieldsOpt @Int ["a", "b"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

    it "errors if intermediate field is the wrong type" $
      decodeWith (getFieldsOpt @Int ["a", "b", "c"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

  describe "getFieldsWith" $ do
    it "decodes field" $
      decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = { b = 1 }" `shouldBe` Right 2

    it "errors if field does not exist" $
      decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = {}" `shouldBe` Left (DecodeError [Key "a", Key "b"] MissingField)

    it "errors if intermediate field does not exist" $
      decodeWith (getFieldsWith @Int succDecoder ["a", "b", "c"]) "a = {}" `shouldBe` Left (DecodeError [Key "a", Key "b"] MissingField)

    it "errors if field is the wrong type" $
      decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

    it "errors if intermediate field is the wrong type" $
      decodeWith (getFieldsWith @Int succDecoder ["a", "b", "c"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

  describe "getFieldsOptWith" $ do
    it "decodes field" $
      decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = { b = 1 }" `shouldBe` Right (Just 2)

    it "returns Nothing if field does not exist" $
      decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = {}" `shouldBe` Right Nothing

    it "returns Nothing if intermediate field does not exist" $
      decodeWith (getFieldsOptWith @Int succDecoder ["a", "b", "c"]) "a = {}" `shouldBe` Right Nothing

    it "errors if field is the wrong type" $
      decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

    it "errors if intermediate field is the wrong type" $
      decodeWith (getFieldsOptWith @Int succDecoder ["a", "b", "c"]) "a = { b = true }" `shouldBe` Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))

  describe "getArrayOf" $ do
    it "decodes field" $
      decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = [1, 2]" `shouldBe` Right [2, 3]

    it "errors if value is the wrong type" $
      decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = true" `shouldBe` Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))

    it "errors if value in array is the wrong type" $
      decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = [1, 2, true]" `shouldBe` Left (DecodeError [Key "a", Index 2] (TypeMismatch (Boolean True)))

  describe "decoding multiple fields at once" $ do
    it "works" $ do
      let decoder :: Decoder (Int, Maybe String, Maybe Int, Bool)
          decoder =
            (,,,)
              <$> getField "a"
              <*> getFieldOpt "b"
              <*> getFieldOpt "missing"
              <*> getFields ["c", "d"]
          doc =
            Text.unlines
              [ "a = 1"
              , "b = \"hello\""
              , "[c]"
              , "d = true"
              ]
      decodeWith decoder doc `shouldBe` Right (1, Just "hello", Nothing, True)

  describe "DecodeTOML instances" $ do
    describe "Bool" $ do
      it "decodes successfully" $ do
        decodeWith (getField @Bool "a") "a = true" `shouldBe` Right True
        decodeWith (getField @Bool "a") "a = false" `shouldBe` Right False

    describe "String" $ do
      it "decodes successfully" $ do
        decodeWith (getField @String "a") "a = 'z'" `shouldBe` Right "z"
        decodeWith (getField @String "a") "a = 'asdf'" `shouldBe` Right "asdf"

    describe "Char" $ do
      it "decodes successfully" $
        decodeWith (getField @Char "a") "a = 'z'" `shouldBe` Right 'z'

      it "errors with multi-character string" $
        decodeWith (getField @Char "a") "a = 'asdf'" `shouldFailWith` "Expected single character string"

    describe "Int" $ do
      it "decodes successfully" $ do
        decodeWith (getField @Int "a") "a = 42" `shouldBe` Right 42
        decodeWith (getField @Int "a") "a = -13" `shouldBe` Right (-13)

    describe "Natural" $ do
      it "decodes successfully" $
        decodeWith (getField @Natural "a") "a = 42" `shouldBe` Right 42

      it "errors with negative numbers" $
        decodeWith (getField @Natural "a") "a = -13" `shouldFailWith` "Got negative number"

    describe "Int8" $ do
      it "decodes successfully" $ do
        decodeWith (getField @Int8 "a") "a = 127" `shouldBe` Right 127
        decodeWith (getField @Int8 "a") "a = 42" `shouldBe` Right 42
        decodeWith (getField @Int8 "a") "a = -13" `shouldBe` Right (-13)
        decodeWith (getField @Int8 "a") "a = -128" `shouldBe` Right (-128)

      it "errors with underflow/overflow" $ do
        decodeWith (getField @Int8 "a") "a = 128" `shouldFailWith` "Overflow"
        decodeWith (getField @Int8 "a") "a = -129" `shouldFailWith` "Underflow"

    describe "Double" $ do
      it "decodes successfully" $ do
        decodeWith (getField @Double "a") "a = 42.0" `shouldBe` Right 42.0
        decodeWith (getField @Double "a") "a = -13.2" `shouldBe` Right (-13.2)

    describe "Array" $
      it "decodes successfully" $ do
        decodeWith (getField @[Int] "a") "a = [1, 2]" `shouldBe` Right [1, 2]

    describe "UTCTime" $
      it "decodes successfully" $ do
        decodeWith (getField @Time.UTCTime "a") "a = 2020-01-31T00:00:01-01:00"
          `shouldBe` Right (Time.UTCTime (Time.fromGregorian 2020 1 31) 3601)

    describe "ZonedTime" $
      it "decodes successfully" $ do
        -- ZonedTime does not have an Eq instance
        decodeWith (getField @Time.ZonedTime "a") "a = 2020-01-31T00:00:01-01:00"
          `shouldSatisfy` P.right
            ( P.con $
                Time.ZonedTime
                  (P.eq $ Time.LocalTime (Time.fromGregorian 2020 1 31) (Time.TimeOfDay 0 0 1))
                  (P.con $ Time.TimeZone (P.eq -60) P.anything P.anything)
            )

    describe "LocalTime" $
      it "decodes successfully" $ do
        decodeWith (getField @Time.LocalTime "a") "a = 2020-01-31T00:00:01"
          `shouldBe` Right (Time.LocalTime (Time.fromGregorian 2020 1 31) (Time.TimeOfDay 0 0 1))

    describe "Day" $
      it "decodes successfully" $ do
        decodeWith (getField @Time.Day "a") "a = 2020-01-31"
          `shouldBe` Right (Time.fromGregorian 2020 1 31)

    describe "TimeOfDay" $
      it "decodes successfully" $ do
        decodeWith (getField @Time.TimeOfDay "a") "a = 07:32:59.1"
          `shouldBe` Right (Time.TimeOfDay 7 32 59.1)

    describe "Maybe" $
      it "decodes successfully" $ do
        decodeWith (getField @(Maybe Int) "a") "a = 1" `shouldBe` Right (Just 1)

    describe "Either" $ do
      it "decodes successfully" $ do
        decodeWith (getField @(Either Int Double) "a") "a = 1" `shouldBe` Right (Left 1)
        decodeWith (getField @(Either Int Double) "a") "a = 1.0" `shouldBe` Right (Right 1)

    describe "()" $
      it "decodes successfully" $ do
        decodeWith (getField @() "a") "a = []" `shouldBe` Right ()

    describe "(a, b)" $ do
      it "decodes successfully" $ do
        decodeWith (getField @(Int, Double) "a") "a = [1, 2.5]" `shouldBe` Right (1, 2.5)

      it "includes index in errors" $
        decodeWith (getField @(Int, Double) "a") "a = [1, true]"
          `shouldSatisfy` P.left (P.con $ DecodeError (P.eq [Key "a", Index 1]) P.anything)

    describe "Map" $ do
      it "decodes successfully" $
        decodeWith (getField @(Map Text Int) "a") "a = {x = 1, y = 2}"
          `shouldBe` Right (Map.fromList [("x", 1), ("y", 2)])

      it "includes key in errors" $
        decodeWith (getField @(Map Text Int) "a") "a = {x = true}"
          `shouldSatisfy` P.left (P.con $ DecodeError (P.eq [Key "a", Key "x"]) P.anything)

shouldFailWith :: Either TOMLError a -> Text -> IO ()
result `shouldFailWith` message = result `shouldSatisfy` P.left decodeError
  where
    decodeError = P.con $ DecodeError P.anything invalidValue
    invalidValue = P.con $ InvalidValue (P.eq message) P.anything
