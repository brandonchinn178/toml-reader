{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TOML.DecodeTest (test) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Decode (
  DecodeTOML,
  Decoder,
  decodeWith,
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

-- TODO: test Boolean
-- TODO: test Char
-- TODO: test String
-- TODO: test Int
-- TODO: test Natural
-- TODO: test Int8
-- TODO: test Double
-- TODO: test Array
-- TODO: test UTCTime
-- TODO: test ZonedTime
-- TODO: test LocalTime
-- TODO: test Day
-- TODO: test TimeOfDay
-- TODO: test Maybe
-- TODO: test Either
-- TODO: test tuples
-- TODO: test errors on deeply nested path
test :: TestTree
test =
  testGroup "TOML.Decode" . concat $
    [ getFieldTests
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
