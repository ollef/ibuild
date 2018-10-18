{-# language DeriveGeneric #-}
module Store where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Hashable(Hashable, hash)

data Hashed v i = Hashed !(v i) !Int
  deriving (Eq, Ord, Show, Generic)

hashed :: Hashable (v i) => v i -> Hashed v i
hashed x = Hashed x (hash x)

instance Hashable (v i) => Hashable (Hashed v i) where
  hash (Hashed _ h) = h

unhashed :: Hashed v i -> v i
unhashed (Hashed x _) = x

data Store s k v = Store
  { storeState :: !s
  , storeCache :: !(DMap k (Hashed v))
  }

instance (GCompare k, Semigroup s) => Semigroup (Store s k v) where
  Store s1 c1 <> Store s2 c2 = Store (s1 <> s2) (c1 <> c2)

instance (GCompare k, Monoid s) => Monoid (Store s k v) where
  mempty = Store mempty mempty

initialise :: GCompare k => s -> Store s k v
initialise s = Store s mempty

getInfo :: Store s k v -> s
getInfo = storeState

putInfo :: s -> Store s k v -> Store s k v
putInfo i s = s { storeState = i }

getValue :: GCompare k => k i -> Store s k v -> Maybe (v i)
getValue k = fmap unhashed . getHashed k

getHashed :: GCompare k => k i -> Store s k v -> Maybe (Hashed v i)
getHashed k = DMap.lookup k . storeCache

putValue :: (Hashable (v i), GCompare k) => k i -> v i -> Store s k v -> Store s k v
putValue k v s = s { storeCache = DMap.insert k (hashed v) $ storeCache s }
