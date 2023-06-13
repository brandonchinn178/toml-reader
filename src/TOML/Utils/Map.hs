{-# LANGUAGE RecordWildCards #-}

module TOML.Utils.Map (
  getPathLens,
  getPath,
) where

import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import TOML.Utils.NonEmpty (zipHistory)

-- |
-- For a non-empty list of keys, iterate through the given 'Map' and return
-- the possibly missing value at the path and a function to set the value at
-- the given path and return the modified input 'Map'.
--
-- @
-- let obj = undefined -- { "a": { "b": { "c": 1 } } }
-- (mValue, setValue) <- getPathLens doRecurse ["a", "b", "c"] obj
--
-- print mValue -- Just 1
-- print (setValue 2) -- { "a": { "b": { "c": 2 } } }
-- @
getPathLens ::
  (Monad m, Ord k) =>
  (NonEmpty k -> Maybe v -> m (Map k v, Map k v -> v))
  -- ^ How to get and set the next Map from the possibly missing value.
  -- Passes in the path taken so far.
  -> NonEmpty k
  -> Map k v
  -> m (Maybe v, v -> Map k v)
getPathLens =
  getPathLensWith (\setVal fromMap -> mkSetter (setVal . fromMap)) (mkSetter id)
  where
    mkSetter setMap k kvs = \v -> setMap $ Map.insert k v kvs

-- | Same as 'getPathLens', except without the setter.
getPath ::
  (Monad m, Ord k) =>
  (NonEmpty k -> Maybe v -> m (Map k v))
  -> NonEmpty k
  -> Map k v
  -> m (Maybe v)
getPath doRecurse path originalMap =
  fst <$> getPathLensWith (\_ _ _ _ -> ()) (\_ _ -> ()) doRecurse' path originalMap
  where
    doRecurse' history mVal = do
      x <- doRecurse history mVal
      pure (x, ())

getPathLensWith ::
  (Monad m, Ord k) =>
  (b -> a -> (k -> Map k v -> b))
  -> (k -> Map k v -> b)
  -> (NonEmpty k -> Maybe v -> m (Map k v, a))
  -> NonEmpty k
  -> Map k v
  -> m (Maybe v, b)
getPathLensWith mkAnn mkFirstAnn doRecurse path originalMap =
  let (_, k) :| ks = zipHistory path
   in foldlM go (buildLens k mkFirstAnn originalMap) ks
  where
    go (mVal, b) (history, k) = do
      (nextMap, a) <- doRecurse history mVal
      pure $ buildLens k (mkAnn b a) nextMap

    buildLens k mkAnn' kvs = (Map.lookup k kvs, mkAnn' k kvs)
