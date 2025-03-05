module TOML.Utils.NonEmptyTest (test) where

import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import TOML.Utils.NonEmpty (zipHistory)

test :: TestTree
test =
  testGroup
    "TOML.Utils.NonEmpty"
    [ zipHistoryTest
    ]

zipHistoryTest :: TestTree
zipHistoryTest =
  testGroup
    "zipHistory"
    [ testCase "should zip already-seen keys with each key" $
        zipHistory (ne ["a", "b", "c"])
          @?= ne
            [ (ne ["a"], "a")
            , (ne ["a", "b"], "b")
            , (ne ["a", "b", "c"], "c")
            ]
    , testCase "works with one-element list" $
        zipHistory (ne ["a"]) @?= ne [(ne ["a"], "a")]
    ]
  where
    ne = NonEmpty.fromList
