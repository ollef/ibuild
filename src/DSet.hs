module DSet where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap

type DSet k = DMap k (Const ())

insert :: GCompare k => k i -> DSet k -> DSet k
insert k = DMap.insert k (Const ())

member :: GCompare k => k i -> DSet k -> Bool
member = DMap.member

notMember :: GCompare k => k i -> DSet k -> Bool
notMember = DMap.notMember
