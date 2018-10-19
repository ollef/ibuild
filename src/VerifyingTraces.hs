{-# language DeriveGeneric #-}
{-# language RankNTypes #-}
module VerifyingTraces where

import Protolude

import Data.Dependent.Map(DMap, GCompare, DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Hashable(Hashable, hash)

data Hashed v i = Hashed !(v i) !Int
  deriving (Eq, Ord, Show, Generic)

hashed :: Hashable (v i) => v i -> Hashed v i
hashed x = Hashed x (hash x)

instance Hashable (Hashed v i) where
  hashWithSalt s (Hashed _ h) = hashWithSalt s h

unhashed :: Hashed v i -> v i
unhashed (Hashed x _) = x

data ValueDeps k v i = ValueDeps
  { value :: !(Hashed v i)
  , dependencies :: !(DMap k (Hashed v))
  }

type VT k v = DMap k (ValueDeps k v)

verify
  :: (Monad m, GCompare k)
  => k i
  -> Hashed v i
  -> (forall i'. k i' -> m (Hashed v i'))
  -> VT k v
  -> m Bool
verify k v fetchHash vt = case DMap.lookup k vt of
  Nothing -> return False
  Just (ValueDeps v' deps)
    | hash v /= hash v' ->
      return False
    | otherwise ->
       allM (DMap.toList deps) $ \(depKey :=> depValue) -> do
        depValue' <- fetchHash depKey
        return $ hash depValue == hash depValue'

allM :: Monad m => [a] -> (a -> m Bool) -> m Bool
allM [] _ = return True
allM (x:xs) p = do
  b <- p x
  if b then
    allM xs p
  else
    return False

record
  :: GCompare k
  => k i
  -> Hashed v i
  -> DMap k (Hashed v)
  -> VT k v
  -> VT k v
record k v deps = DMap.insert k (ValueDeps v deps)
