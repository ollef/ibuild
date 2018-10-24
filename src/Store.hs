module Store where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap

data Store s k v = Store
  { storeState :: !s
  , storeCache :: !(DMap k v)
  } deriving Show

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

getValue :: GCompare k => k i -> Store s k v -> v i
getValue k = fromMaybe (panic "getValue: No such key") . DMap.lookup k . storeCache

putValue :: GCompare k => k i -> v i -> Store s k v -> Store s k v
putValue k v s = s { storeCache = DMap.insert k v $ storeCache s }
