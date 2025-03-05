module TOML.Utils.NonEmptySpec (spec) where

import Data.List.NonEmpty qualified as NonEmpty
import Skeletest

import TOML.Utils.NonEmpty (zipHistory)

spec :: Spec
spec = do
  describe "zipHistory" $ do
    it "should zip already-seen keys with each key" $
      zipHistory (ne ["a", "b", "c"])
        `shouldBe` ne
          [ (ne ["a"], "a")
          , (ne ["a", "b"], "b")
          , (ne ["a", "b", "c"], "c")
          ]
    it "works with one-element list" $
      zipHistory (ne ["a"]) `shouldBe` ne [(ne ["a"], "a")]

ne :: [a] -> NonEmpty.NonEmpty a
ne = NonEmpty.fromList
