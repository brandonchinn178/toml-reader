{-# LANGUAGE LambdaCase #-}

module TOML.Utils.MapTest (test) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Utils.Map (getPath, getPathLens)

test :: TestTree
test =
  testGroup
    "TOML.Utils.Map"
    [ getPathLensTest
    , getPathTest
    ]

data MapOrInt = Map [(String, MapOrInt)] | Int Int
  deriving (Show, Eq)

recurseMapOrInt ::
  NonEmpty String
  -> Maybe MapOrInt
  -> Either String (Map String MapOrInt, Map String MapOrInt -> MapOrInt)
recurseMapOrInt _ = \case
  Nothing -> Right (Map.empty, fromMap)
  Just (Map kvs) -> Right (Map.fromList kvs, fromMap)
  Just (Int x) -> Left $ "Could not recurse on: " ++ show x
  where
    fromMap = Map . Map.toList

getPathLensTest :: TestTree
getPathLensTest =
  testGroup
    "getPathLens"
    [ testCase "gets and sets value at path" $ do
        let obj = Map.fromList [("a", Map [("b", Map [("c", Int 1)])])]

        Right (mValue, setValue) <- pure $ getPathLens recurseMapOrInt (NonEmpty.fromList ["a", "b", "c"]) obj

        mValue @?= Just (Int 1)

        let newVal = Map [("foo", Int 100)]
        setValue newVal @?= Map.fromList [("a", Map [("b", Map [("c", newVal)])])]
    ]

getPathTest :: TestTree
getPathTest =
  testGroup
    "getPath"
    [ testCase "gets value at path" $ do
        let obj = Map.fromList [("a", Map [("b", Map [("c", Int 1)])])]

        let doRecurse history = fmap fst . recurseMapOrInt history
        Right mValue <- pure $ getPath doRecurse (NonEmpty.fromList ["a", "b", "c"]) obj

        mValue @?= Just (Int 1)
    ]
