{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TOML.DecodeTest (test) where

import Data.Int (Int8)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Decode (
  DecodeTOML,
  Decoder,
  decodeWith,
  getArrayOf,
  getField,
  getFieldOpt,
  getFieldOptWith,
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

test :: TestTree
test =
  testGroup "TOML.Decode" . concat $
    [ getFieldTests
    , [decoderInstanceTests]
    ]

getFieldTests :: [TestTree]
getFieldTests =
  [ testGroup
      "getField"
      [ testCase "decodes field" $
          decodeWith (getField @Int "a") "a = 1" @?= Right 1
      , testCase "errors if field does not exist" $
          decodeWith (getField @Int "a") "" @?= Left (DecodeError [Key "a"] MissingField)
      , testCase "errors if field is the wrong type" $
          decodeWith (getField @Int "a") "a = true" @?= Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldOpt"
      [ testCase "decodes field" $
          decodeWith (getFieldOpt @Int "a") "a = 1" @?= Right (Just 1)
      , testCase "returns Nothing if field does not exist" $
          decodeWith (getFieldOpt @Int "a") "" @?= Right Nothing
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldOpt @Int "a") "a = true" @?= Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldWith"
      [ testCase "decodes field" $
          decodeWith (getFieldWith @Int succDecoder "a") "a = 1" @?= Right 2
      , testCase "errors if field does not exist" $
          decodeWith (getFieldWith @Int succDecoder "a") "" @?= Left (DecodeError [Key "a"] MissingField)
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldWith @Int succDecoder "a") "a = true" @?= Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldOptWith"
      [ testCase "decodes field" $
          decodeWith (getFieldOptWith @Int succDecoder "a") "a = 1" @?= Right (Just 2)
      , testCase "returns Nothing if field does not exist" $
          decodeWith (getFieldOptWith @Int succDecoder "a") "" @?= Right Nothing
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldOptWith @Int succDecoder "a") "a = true" @?= Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFields"
      [ testCase "decodes field" $
          decodeWith (getFields @Int ["a", "b"]) "a = { b = 1 }" @?= Right 1
      , testCase "errors if field does not exist" $
          decodeWith (getFields @Int ["a", "b"]) "a = {}" @?= Left (DecodeError [Key "a", Key "b"] MissingField)
      , testCase "errors if intermediate field does not exist" $
          decodeWith (getFields @Int ["a", "b", "c"]) "a = {}" @?= Left (DecodeError [Key "a", Key "b"] MissingField)
      , testCase "errors if field is the wrong type" $
          decodeWith (getFields @Int ["a", "b"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      , testCase "errors if intermediate field is the wrong type" $
          decodeWith (getFields @Int ["a", "b", "c"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldsOpt"
      [ testCase "decodes field" $
          decodeWith (getFieldsOpt @Int ["a", "b"]) "a = { b = 1 }" @?= Right (Just 1)
      , testCase "returns Nothing if field does not exist" $
          decodeWith (getFieldsOpt @Int ["a", "b"]) "a = {}" @?= Right Nothing
      , testCase "returns Nothing if intermediate field does not exist" $
          decodeWith (getFieldsOpt @Int ["a", "b", "c"]) "a = {}" @?= Right Nothing
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldsOpt @Int ["a", "b"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      , testCase "errors if intermediate field is the wrong type" $
          decodeWith (getFieldsOpt @Int ["a", "b", "c"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldsWith"
      [ testCase "decodes field" $
          decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = { b = 1 }" @?= Right 2
      , testCase "errors if field does not exist" $
          decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = {}" @?= Left (DecodeError [Key "a", Key "b"] MissingField)
      , testCase "errors if intermediate field does not exist" $
          decodeWith (getFieldsWith @Int succDecoder ["a", "b", "c"]) "a = {}" @?= Left (DecodeError [Key "a", Key "b"] MissingField)
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldsWith @Int succDecoder ["a", "b"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      , testCase "errors if intermediate field is the wrong type" $
          decodeWith (getFieldsWith @Int succDecoder ["a", "b", "c"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getFieldsOptWith"
      [ testCase "decodes field" $
          decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = { b = 1 }" @?= Right (Just 2)
      , testCase "returns Nothing if field does not exist" $
          decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = {}" @?= Right Nothing
      , testCase "returns Nothing if intermediate field does not exist" $
          decodeWith (getFieldsOptWith @Int succDecoder ["a", "b", "c"]) "a = {}" @?= Right Nothing
      , testCase "errors if field is the wrong type" $
          decodeWith (getFieldsOptWith @Int succDecoder ["a", "b"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      , testCase "errors if intermediate field is the wrong type" $
          decodeWith (getFieldsOptWith @Int succDecoder ["a", "b", "c"]) "a = { b = true }" @?= Left (DecodeError [Key "a", Key "b"] (TypeMismatch (Boolean True)))
      ]
  , testGroup
      "getArrayOf"
      [ testCase "decodes field" $
          decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = [1, 2]" @?= Right [2, 3]
      , testCase "errors if value is the wrong type" $
          decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = true" @?= Left (DecodeError [Key "a"] (TypeMismatch (Boolean True)))
      , testCase "errors if value in array is the wrong type" $
          decodeWith (getFieldWith (getArrayOf @Int succDecoder) "a") "a = [1, 2, true]" @?= Left (DecodeError [Key "a", Index 2] (TypeMismatch (Boolean True)))
      ]
  , testCase "Decoding multiple fields at once" $ do
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
      decodeWith decoder doc @?= Right (1, Just "hello", Nothing, True)
  ]
  where
    succDecoder :: (Enum a, DecodeTOML a) => Decoder a
    succDecoder = succ <$> tomlDecoder

decoderInstanceTests :: TestTree
decoderInstanceTests =
  testGroup
    "DecodeTOML instances"
    [ testCase "Bool" $ do
        decodeWith (getField @Bool "a") "a = true" @?= Right True
        decodeWith (getField @Bool "a") "a = false" @?= Right False
    , testCase "String" $ do
        decodeWith (getField @String "a") "a = 'z'" @?= Right "z"
        decodeWith (getField @String "a") "a = 'asdf'" @?= Right "asdf"
    , testCase "Char" $
        decodeWith (getField @Char "a") "a = 'z'" @?= Right 'z'
    , testCase "Char errors with multi-character string" $
        assertInvalidValue "Expected single character string" $
          decodeWith (getField @Char "a") "a = 'asdf'"
    , testCase "Int" $ do
        decodeWith (getField @Int "a") "a = 42" @?= Right 42
        decodeWith (getField @Int "a") "a = -13" @?= Right (-13)
    , testCase "Natural" $
        decodeWith (getField @Natural "a") "a = 42" @?= Right 42
    , testCase "Natural errors with negative numbers" $
        assertInvalidValue "Got negative number" $
          decodeWith (getField @Natural "a") "a = -13"
    , testCase "Int8" $ do
        decodeWith (getField @Int8 "a") "a = 127" @?= Right 127
        decodeWith (getField @Int8 "a") "a = 42" @?= Right 42
        decodeWith (getField @Int8 "a") "a = -13" @?= Right (-13)
        decodeWith (getField @Int8 "a") "a = -128" @?= Right (-128)
    , testCase "Int8 errors with underflow/overflow" $ do
        assertInvalidValue "Overflow" $
          decodeWith (getField @Int8 "a") "a = 128"
        assertInvalidValue "Underflow" $
          decodeWith (getField @Int8 "a") "a = -129"
    , testCase "Double" $ do
        decodeWith (getField @Double "a") "a = 42.0" @?= Right 42.0
        decodeWith (getField @Double "a") "a = -13.2" @?= Right (-13.2)
    , testCase "Array" $
        decodeWith (getField @[Int] "a") "a = [1, 2]" @?= Right [1, 2]
    , testCase "UTCTime" $
        decodeWith (getField @Time.UTCTime "a") "a = 2020-01-31T00:00:01-01:00"
          @?= Right (Time.UTCTime (Time.fromGregorian 2020 1 31) 3601)
    , testCase "ZonedTime" $
        -- ZonedTime does not have an Eq instance
        case decodeWith (getField @Time.ZonedTime "a") "a = 2020-01-31T00:00:01-01:00" of
          Right (Time.ZonedTime (Time.LocalTime day time) (Time.TimeZone (-60) _ _))
            | day == Time.fromGregorian 2020 1 31
            , time == Time.TimeOfDay 0 0 1 ->
                return ()
          result -> unexpectedResult result
    , testCase "LocalTime" $
        decodeWith (getField @Time.LocalTime "a") "a = 2020-01-31T00:00:01"
          @?= Right (Time.LocalTime (Time.fromGregorian 2020 1 31) (Time.TimeOfDay 0 0 1))
    , testCase "Day" $
        decodeWith (getField @Time.Day "a") "a = 2020-01-31"
          @?= Right (Time.fromGregorian 2020 1 31)
    , testCase "TimeOfDay" $
        decodeWith (getField @Time.TimeOfDay "a") "a = 07:32:59.1"
          @?= Right (Time.TimeOfDay 7 32 59.1)
    , testCase "Maybe" $
        decodeWith (getField @(Maybe Int) "a") "a = 1" @?= Right (Just 1)
    , testCase "Either" $ do
        decodeWith (getField @(Either Int Double) "a") "a = 1" @?= Right (Left 1)
        decodeWith (getField @(Either Int Double) "a") "a = 1.0" @?= Right (Right 1)
    , testCase "()" $
        decodeWith (getField @() "a") "a = []" @?= Right ()
    , testCase "(a, b)" $
        decodeWith (getField @(Int, Double) "a") "a = [1, 2.5]" @?= Right (1, 2.5)
    , testCase "Tuples show errors with index" $
        case decodeWith (getField @(Int, Double) "a") "a = [1, true]" of
          Left (DecodeError [Key "a", Index 1] _) -> return ()
          result -> unexpectedResult result
    ]
  where
    assertInvalidValue expected result =
      case result of
        Left (DecodeError _ (InvalidValue actual _)) -> actual @?= expected
        _ -> unexpectedResult result

    unexpectedResult result = assertFailure $ "Got unexpected result: " <> show result
