{-# LANGUAGE LambdaCase #-}

module TOML.Utils.MapSpec (spec) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Skeletest

import TOML.Utils.Map (getPath, getPathLens)

spec :: Spec
spec = do
  describe "getPathLens" $ do
    it "gets and sets value at path" $ do
      let obj = Map.fromList [("a", Map [("b", Map [("c", Int 1)])])]

      Right (mValue, setValue) <- pure $ getPathLens recurseMapOrInt (NonEmpty.fromList ["a", "b", "c"]) obj

      mValue `shouldBe` Just (Int 1)

      let newVal = Map [("foo", Int 100)]
      setValue newVal `shouldBe` Map.fromList [("a", Map [("b", Map [("c", newVal)])])]

  describe "getPath" $ do
    it "gets value at path" $ do
      let obj = Map.fromList [("a", Map [("b", Map [("c", Int 1)])])]

      let doRecurse history = fmap fst . recurseMapOrInt history
      Right mValue <- pure $ getPath doRecurse (NonEmpty.fromList ["a", "b", "c"]) obj

      mValue `shouldBe` Just (Int 1)

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
