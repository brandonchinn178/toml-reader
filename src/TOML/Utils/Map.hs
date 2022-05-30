{-# LANGUAGE RecordWildCards #-}

module TOML.Utils.Map (
  getPathLens,
) where

import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import TOML.Utils.NonEmpty (zipHistory)

{- |
For a non-empty list of keys, iterate through the given 'Map' and return
the possibly missing value at the path and a function to set the value at
the given path and return the modified input 'Map'.

@
let obj = undefined -- { "a": { "b": { "c": 1 } } }
(mValue, setValue) <- getPathLens doRecurse ["a", "b", "c"] obj

print mValue -- Just 1
print (setValue 2) -- { "a": { "b": { "c": 2 } } }
@
-}
getPathLens ::
  (Monad m, Ord k) =>
  -- | How to get and set the next Map from the possibly missing value.
  -- Passes in the path taken so far.
  (NonEmpty k -> Maybe v -> m (Map k v, Map k v -> v)) ->
  NonEmpty k ->
  Map k v ->
  m (Maybe v, v -> Map k v)
getPathLens doRecurse path originalMap =
  let (_, k) :| ks = zipHistory path
   in foldlM go (buildLens k id originalMap) ks
  where
    go (mVal, setVal) (history, k) = do
      (nextMap, fromMap) <- doRecurse history mVal
      pure $ buildLens k (setVal . fromMap) nextMap

    buildLens k setMap kvs = (Map.lookup k kvs, \v -> setMap $ Map.insert k v kvs)
