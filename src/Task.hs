{-# language ConstraintKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Task where

import Protolude

import Control.Monad.State
import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show
import Text.Show

import DSet(DSet)
import qualified DSet
import Hashed
import Store
import VerifyingTraces(VT)
import qualified VerifyingTraces as VT

newtype Task c k v a = Task
  { runTask
  :: forall f. (c f, Monad f)
  => (forall i. k i -> f (v i))
  -> f a
  }

fetch :: k i -> Task c k v (v i)
fetch k = Task $ \f -> f k

liftAction :: (forall f. (c f, Monad f) => f a) -> Task c k v a
liftAction fa = Task $ \_ -> fa

instance Functor (Task c k v) where
  fmap f (Task g) = Task $ \fetch_ -> map f $ g fetch_

instance Applicative (Task c k v) where
  pure a = Task $ \_ -> pure a
  Task f <*> Task x = Task $ \fetch_ -> f fetch_ <*> x fetch_

instance Monad (Task c k v) where
  Task a >>= f = Task $ \fetch_ -> do
    x <- a fetch_
    runTask (f x) fetch_

type Tasks c k v = forall i. k i -> Task c k v (v i)

type Build c s k v = forall i. Tasks c k v -> k i -> Store s k v -> Store s k v

type Rebuilder c s k v = forall i. k i -> v i -> Task c k v (v i) -> Task (MonadState s) k v (v i)

type Scheduler c s sr k v = Rebuilder c sr k v -> Build c s k v

perpetualRebuilder :: Rebuilder (MonadState s) s k v
perpetualRebuilder _k _v task = Task $ runTask task

track
  :: (GCompare k, Monad m)
  => Task Monad k v a
  -> (forall i'. k i' -> m (v i'))
  -> m (a, DMap k v)
track task fetch_ = flip runStateT mempty $ runTask task $ \k -> do
  v <- lift $ fetch_ k
  modify' $ DMap.insert k v
  return v

busy
  :: forall s k v
  . GCompare k
  => Build Monad s k v
busy (tasks :: Tasks Monad k v) key store = execState (fetch_ key) store
  where
    fetch_ :: k i -> State (Store s k v) (v i)
    fetch_ k = do
      v <- runTask (tasks k) fetch_
      modify $ putValue k v
      return v

suspending
  :: forall s k v
  . GCompare k
  => Scheduler Monad s s k v
suspending (rebuilder :: Rebuilder Monad s k v) (tasks :: Tasks Monad k v) target store
  = fst $ execState (fetch_ target) (store, mempty)
  where
    fetch_ :: forall i. k i -> State (Store s k v, DSet k) (v i)
    fetch_ key = do
      done <- gets snd
      if key `DSet.notMember` done then do
        value <- gets $ getValue key . fst
        let newTask = rebuilder key value $ tasks key
        newValue <- liftRun newTask fetch_
        modify $ \(s, d) -> (putValue key newValue s, DSet.insert key d)
        return newValue
      else
        gets $ getValue key . fst

liftRun
  :: Task (MonadState s) k v a
  -> (forall i. k i -> State (Store s k v, extra) (v i))
  -> State (Store s k v, extra) a
liftRun t f = unwrap $ runTask t (Wrap . f)

newtype Wrap s extra k v a = Wrap { unwrap :: State (Store s k v, extra) a }
  deriving (Functor, Applicative, Monad)

instance MonadState s (Wrap s extra k v) where
  get = Wrap $ gets (getInfo . fst)
  put s = Wrap $ modify $ \(store, extra) -> (putInfo s store, extra)

vtRebuilder :: (GCompare k, HashTag k v) => Rebuilder Monad (VT k v) k v
vtRebuilder key value task = Task $ \fetch_ -> do
  vt <- get
  upToDate <- VT.verify key (hashed key value) (\k -> hashed k <$> fetch_ k) vt
  if upToDate then
    return value
  else do
    (newValue, deps) <- track task fetch_
    modify $ VT.record key (hashed key newValue) $ DMap.mapWithKey hashed deps
    return newValue

shake :: (GCompare k, HashTag k v) => Build Monad (VT k v) k v
shake = suspending vtRebuilder

-------------------------------------------------------------------------------
data ModuleName = ModuleName
  deriving (Show, Generic)
instance Hashable ModuleName
data ModuleHeader = ModuleHeader ModuleName
  deriving (Show, Generic)
instance Hashable ModuleHeader
data ParsedModule = ParsedModule ModuleHeader
  deriving (Show, Generic)
instance Hashable ParsedModule

data TaskKey a where
  ParseModuleHeader :: ModuleName -> TaskKey (ModuleHeader, Text)
  ParseModule :: ModuleName -> TaskKey ParsedModule

deriving instance Show (TaskKey a)

instance HashTag TaskKey Identity where
  hashTagged ParseModuleHeader {} = hash
  hashTagged ParseModule {} = hash

type CompilerTask = Task Monad TaskKey Identity
type CompilerTasks = Tasks Monad TaskKey Identity

compilerTasks :: CompilerTasks
compilerTasks (ParseModuleHeader mname) = Identity <$> parseModuleHeader mname
compilerTasks (ParseModule mname) = Identity <$> parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = pure (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname = do
  Identity (header, _t) <- fetch (ParseModuleHeader mname)
  pure $ ParsedModule header

-------------------------------------------------------------------------------
data SheetKey a where
  A :: SheetKey Integer
  B :: SheetKey Integer
  C :: SheetKey Integer
  D :: SheetKey Integer

instance GEq SheetKey where
  geq a b = case gcompare a b of
    GLT -> Nothing
    GEQ -> Just Refl
    GGT -> Nothing

instance GCompare SheetKey where
  gcompare A A = GEQ
  gcompare B B = GEQ
  gcompare C C = GEQ
  gcompare D D = GEQ
  gcompare A _ = GLT
  gcompare _ A = GGT
  gcompare B _ = GLT
  gcompare _ B = GGT
  gcompare C _ = GLT
  gcompare _ C = GGT

deriving instance Show (SheetKey a)

instance HashTag SheetKey Identity where
  hashTagged A = hash
  hashTagged B = hash
  hashTagged C = hash
  hashTagged D = hash

instance GShow SheetKey where
  gshowsPrec = showsPrec

instance ShowTag SheetKey Identity where
  showTaggedPrec A = showsPrec
  showTaggedPrec B = showsPrec
  showTaggedPrec C = showsPrec
  showTaggedPrec D = showsPrec

type SheetTask = Task Monad SheetKey Identity
type SheetTasks = Tasks Monad SheetKey Identity

sheetTasks :: SheetTasks
sheetTasks A = trace ("computing A" :: Text) $ pure $ Identity 10
sheetTasks B = trace ("computing B" :: Text) $ do
  Identity a <- fetch A
  pure $ Identity $ a + 20
sheetTasks C = trace ("computing C" :: Text) $ do
  Identity a <- fetch A
  pure $ Identity $ a + 30
sheetTasks D = trace ("computing D" :: Text) $ do
  Identity b <- fetch B
  Identity c <- fetch C
  pure $ Identity $ b + c
