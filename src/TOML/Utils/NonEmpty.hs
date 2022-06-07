{-# LANGUAGE TupleSections #-}

module TOML.Utils.NonEmpty (
  zipHistory,
) where

import Data.List (scanl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty

{- |
Annotates each element with the history of all past elements.

>>> zipHistory ["a", "b", "c"]
[(["a"], "a"), (["a", "b"], "b"), (["a", "b", "c"], "c")]
-}
zipHistory :: NonEmpty a -> NonEmpty (NonEmpty a, a)
zipHistory (a :| as) =
  NonEmpty.fromList $
    scanl'
      (\(history, _) x -> (append history x, x))
      (singleton a, a)
      as
  where
    append xs x = xs <> singleton x
    -- NonEmpty.singleton was added in base 4.15
    singleton x = x :| []
