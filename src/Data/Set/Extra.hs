{-# LANGUAGE BangPatterns #-}

module Data.Set.Extra
  ( partitionM
  , selectM
  )

where

import Protolude.Lifted

import qualified Data.Set as Set

partitionM
  :: (Monad m, Ord a)
  => (a -> m Bool) -> Set a -> m (Set a, Set a)
partitionM p = Set.foldr (selectM p) (return (Set.empty, Set.empty))

selectM
  :: (Monad m, Ord a)
  => (a -> m Bool) -> a -> m (Set a, Set a) -> m (Set a, Set a)
selectM p x tfs = do
  !v <- p x
  (!ts, !fs) <- tfs
  if v
    then return (Set.insert x ts, fs)
    else return (ts, Set.insert x fs)
