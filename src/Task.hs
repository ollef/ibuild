{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
module Task where

import Protolude

import Control.Monad.Writer
import Data.Dependent.Map(GCompare, Some(This), DSum((:=>)))

import Store

newtype Task c k v a = Task
  { runTask
  :: forall f. c f
  => (forall i. k i -> f (v i))
  -> f a
  }

fetch :: k i -> Task c k v (v i)
fetch k = Task $ \f -> f k

instance (forall f. c f => Functor f) => Functor (Task c k v) where
  fmap f (Task g) = Task $ \fetch_ -> map f $ g fetch_
  a <$ Task g = Task $ \fetch_ -> a <$ g fetch_

instance (forall f. c f => Applicative f) => Applicative (Task c k v) where
  pure x = Task $ \_ -> pure x
  Task f <*> Task x = Task $ \fetch_ -> f fetch_ <*> x fetch_
  liftA2 f (Task x) (Task y) = Task $ \fetch_ -> liftA2 f (x fetch_) (y fetch_)
  Task x *> Task y = Task $ \fetch_ -> x fetch_ *> y fetch_
  Task x <* Task y = Task $ \fetch_ -> x fetch_ <* y fetch_

instance (forall f. c f => Monad f) => Monad (Task c k v) where
  Task a >>= f = Task $ \fetch_ -> do
    x <- a fetch_
    runTask (f x) fetch_
  Task a >> Task b = Task $ \fetch_ -> do
    a fetch_ >> b fetch_
  return x = Task $ \_ -> pure x
  fail x = Task $ \_ -> fail x

type Tasks c k v = forall i. k i -> Task c k v (v i)

type Build c s k v = forall i. Tasks c k v -> k i -> Store s k v -> Store s k v

type Rebuilder c s k v = forall i. k i -> v i -> Task c k v (v i) -> Task (MonadState s) k v (v i)

type Scheduler c s sr k v = Rebuilder c sr k v -> Build c s k v

perpetualRebuilder :: (forall f. MonadState s f => c f) => Rebuilder c s k v
perpetualRebuilder _k _v task = Task $ runTask task

dependencies :: (forall f. Applicative f => c f) => Task c k v i -> [Some k]
dependencies task = getConst $ runTask task (\k -> Const [This k])

track
  :: forall m k v a c
  . (Monad m, forall f. Functor f => c f)
  => Task c k v a
  -> (forall i'. k i' -> m (v i'))
  -> m (a, [DSum k v])
track task fetch_ = runWriterT $ runTask task $ \k -> do
  v <- lift $ fetch_ k
  tell [k :=> v]
  return v

busy :: forall c s k v. (GCompare k, forall i. Hashable (v i), forall f. Functor f => c f) => Build c s k v
busy (tasks :: Tasks c k v) key store = execState (fetch_ key) store
  where
    fetch_ :: k i -> State (Store s k v) (v i)
    fetch_ k = do
      mv <- gets $ getValue k
      case mv of
        Just v -> return v
        Nothing -> do
          v <- runTask (tasks k) fetch_
          modify $ putValue k v
          return v


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
compilerTasks (ParseModuleHeader mname) = Identity <$> parseModuleHeader mname
compilerTasks (ParseModule mname) = Identity <$> parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = return (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname = do
  Identity (header, _t) <- fetch $ ParseModuleHeader mname
  pure $ ParsedModule header
