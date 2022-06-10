{-# LANGUAGE OverloadedStrings #-}

module TOML.ParserTest (test) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Parser (parseTOML)
import TOML.Value (Value (..))

test :: TestTree
test =
  testGroup
    "TOML.Parser"
    [ testCase "Large floats are parsed efficiently" $
        case parseTOML "" "a = 1e1000000000000000000000000000000000000000000000000000000000000000" of
          Right (Table t) -> Map.lookup "a" t @?= Just (Float (read "Infinity"))
          result -> assertFailure $ "Got unexpected result: " ++ show result
    ]
