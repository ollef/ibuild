{-# LANGUAGE ConstraintKinds, GADTs, OverloadedStrings, StandaloneDeriving, TypeFamilies, RankNTypes #-}
module Task where

import Protolude

import Control.Monad.Writer
import Data.Dependent.Map(DMap, Some(This), DSum((:=>)), GCompare)
import qualified Data.Dependent.Map as DMap

newtype Task c k v i = Task
  { run
  :: forall f. c f
  => (forall i'. k i' -> f (v i'))
  -> f (v i)
  }

pureTask :: v i -> Task Applicative k v i
pureTask x = Task $ \_ -> pure x

type Tasks c k v = forall i. k i -> Task c k v i

data Store s k v = Store
  { storeState :: s
  , storeCache :: DMap k v
  }

compute :: GCompare k => Task Monad k v i -> Store s k v -> v i
compute task store = runIdentity $ run task $ \k -> Identity (storeCache store DMap.! k)

type Build c s k v = forall i. Tasks c k v -> k i -> Store s k v -> Store s k v

type Rebuilder c s k v = forall i. k i -> v i -> Task c k v i -> Task (MonadState s) k v i

type Scheduler c s sr k v = Rebuilder c sr k v -> Build c s k v

perpetualRebuilder :: Rebuilder Monad () k v
perpetualRebuilder _k _v task = Task $ run task

dependencies :: Task Applicative k v i -> [Some k]
dependencies task = getConst $ run task (\k -> Const [This k])

track
  :: forall m k v i. Monad m
  => Task Monad k v i
  -> (forall i'. k i' -> m (v i'))
  -> m (v i, [DSum k v])
track task fetch = runWriterT $ run task $ \k -> do
  v <- lift $ fetch k
  tell [k :=> v]
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
compilerTasks (ParseModuleHeader mname) = parseModuleHeader mname
compilerTasks (ParseModule mname) = parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = Task $ \_ -> pure $ Identity (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname = Task $ \fetch -> do
  Identity (header, _t) <- fetch $ ParseModuleHeader mname
  pure $ Identity $ ParsedModule header
