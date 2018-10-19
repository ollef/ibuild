{-# language ConstraintKinds #-}
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
import Data.Dependent.Map(DMap, GCompare, Some(This))
import qualified Data.Dependent.Map as DMap

import DSet(DSet)
import qualified DSet
import Store
import VerifyingTraces(VT)
import qualified VerifyingTraces as VT
import Hashed

infixl 1 >>=.
infixl 4 <*>.
infixl 4 *>.
infixl 4 <$>.

newtype Task c k v a = Task
  { runTask
  :: forall f. c f
  => (forall i. k i -> f (v i))
  -> f a
  }

fetch :: k i -> Task c k v (v i)
fetch k = Task $ \f -> f k

liftAction :: (forall f. c f => f a) -> Task c k v a
liftAction fa = Task $ \_ -> fa

map_ :: (forall f. c f => Functor f) => (a -> b) -> Task c k v a -> Task c k v b
map_ f (Task g) = Task $ \fetch_ -> map f $ g fetch_

(<$>.) :: (forall f. c f => Functor f) => (a -> b) -> Task c k v a -> Task c k v b
(<$>.) = map_

pure_ :: (forall f. c f => Applicative f) => a -> Task c k v a
pure_ a = Task $ \_ -> pure a

(<*>.)
  :: (forall f. c f => Applicative f)
  => Task c k v (a -> b)
  -> Task c k v a
  -> Task c k v b
Task f <*>. Task x = Task $ \fetch_ -> f fetch_ <*> x fetch_

(*>.)
  :: (forall f. c f => Applicative f)
  => Task c k v a
  -> Task c k v b
  -> Task c k v b
Task a *>. Task b = Task $ \fetch_ -> a fetch_ *> b fetch_

(>>=.)
  :: (forall f. c f => Monad f)
  => Task c k v a
  -> (a -> Task c k v b)
  -> Task c k v b
Task a >>=. f = Task $ \fetch_ -> do
  x <- a fetch_
  runTask (f x) fetch_

type Tasks c k v = forall i. k i -> Maybe (Task c k v (v i))

type Build c s k v = forall i. Tasks c k v -> k i -> Store s k v -> Store s k v

type Rebuilder c s k v = forall i. k i -> v i -> Task c k v (v i) -> Task (MonadState s) k v (v i)

type Scheduler c s sr k v = Rebuilder c sr k v -> Build c s k v

perpetualRebuilder :: (forall f. MonadState s f => c f) => Rebuilder c s k v
perpetualRebuilder _k _v task = Task $ runTask task

dependencies :: (forall f. Applicative f => c f) => Task c k v i -> [Some k]
dependencies task = getConst $ runTask task (\k -> Const [This k])

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
  :: forall c s k v
  . (GCompare k, forall i. Hashable (v i), forall f. Functor f => c f)
  => Build c s k v
busy (tasks :: Tasks c k v) key store = execState (fetch_ key) store
  where
    fetch_ :: k i -> State (Store s k v) (v i)
    fetch_ k = do
      case tasks k of
        Nothing -> gets $ getValue k
        Just task -> do
          v <- runTask task fetch_
          modify $ putValue k v
          return v

suspending
  :: forall s k v
  . (GCompare k, forall i. Hashable (v i))
  => Scheduler Monad s s k v
suspending (rebuilder :: Rebuilder Monad s k v) (tasks :: Tasks Monad k v) target store
  = fst $ execState (fetch_ target) (store, mempty)
  where
    fetch_ :: forall i. k i -> State (Store s k v, DSet k) (v i)
    fetch_ key = do
      done <- gets snd
      case tasks key of
        Just task | key `DSet.notMember` done -> do
          value <- gets $ getValue key . fst
          let newTask = rebuilder key value task
          newValue <- liftRun newTask fetch_
          modify $ \(s, d) -> (putValue key newValue s, DSet.insert key d)
          return newValue
        _ -> gets $ getValue key . fst

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

vtRebuilder :: (GCompare k, forall i. Hashable (v i)) => Rebuilder Monad (VT k v) k v
vtRebuilder key value task = Task $ \fetch_ -> do
  vt <- get
  upToDate <- VT.verify key (hashed value) (map hashed . fetch_) vt
  if upToDate then
    return value
  else do
    (newValue, deps) <- track task fetch_
    modify $ VT.record key (hashed newValue) $ DMap.map hashed deps
    return newValue

shake :: (GCompare k, forall i. Hashable (v i)) => Build Monad (VT k v) k v
shake = suspending vtRebuilder

-------------------------------------------------------------------------------
data ModuleName = ModuleName
  deriving Show
data ModuleHeader = ModuleHeader ModuleName
  deriving Show
data ParsedModule = ParsedModule ModuleHeader
  deriving Show

data TaskKey a where
  ParseModuleHeader :: ModuleName -> TaskKey (ModuleHeader, Text)
  ParseModule :: ModuleName -> TaskKey ParsedModule

deriving instance Show (TaskKey a)

type CompilerTask = Task Monad TaskKey Identity
type CompilerTasks = Tasks Monad TaskKey Identity

compilerTasks :: CompilerTasks
compilerTasks (ParseModuleHeader mname) = Just $ Identity <$>. parseModuleHeader mname
compilerTasks (ParseModule mname) = Just $ Identity <$>. parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = pure_ (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname =
  fetch (ParseModuleHeader mname) >>=. \(Identity (header, _t)) ->
  pure_ $ ParsedModule header
