{-# LANGUAGE OverloadedStrings #-}

module TOML.DecodeTest (test) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Decode (decodeWith, getField)

test :: TestTree
test =
  testGroup
    "TOML.Decode"
    [ testCase "getField" $
        decodeWith (getField "a") "a = 1" @?= Right (1 :: Int)
    ]
