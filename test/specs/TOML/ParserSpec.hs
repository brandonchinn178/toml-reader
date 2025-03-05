{-# LANGUAGE OverloadedStrings #-}

module TOML.ParserSpec (spec) where

import Data.Map.Strict qualified as Map
import Skeletest

import TOML.Parser (parseTOML)
import TOML.Value (Value (..))

spec :: Spec
spec = do
  it "parses large floats efficiently" $ do
    let input = "a = 1e1000000000000000000000000000000000000000000000000000000000000000"
    parseTOML "" input `shouldBe` Right (Table $ Map.singleton "a" (Float $ read "Infinity"))
