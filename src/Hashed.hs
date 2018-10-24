{-# language FlexibleContexts #-}
module Hashed(Hashed, hashed, unhashed, Keyed(Keyed)) where

import Protolude

data Hashed v i = Hashed !(v i) !Int
  deriving (Show)

instance Eq (v i) => Eq (Hashed v i) where
  Hashed v1 h1 == Hashed v2 h2 = h1 == h2 && v1 == v2

instance Ord (v i) => Ord (Hashed v i) where
  compare (Hashed v1 _) (Hashed v2 _) = compare v1 v2

instance Hashable (Hashed v i) where
  hashWithSalt s (Hashed _ h) = hashWithSalt s h

unhashed :: Hashed v i -> v i
unhashed (Hashed x _) = x

data Keyed k v i = Keyed !(k i) !(v i)

hashed :: Hashable (Keyed k v i) => k i -> v i -> Hashed v i
hashed k v = Hashed v $ hash $ Keyed k v
