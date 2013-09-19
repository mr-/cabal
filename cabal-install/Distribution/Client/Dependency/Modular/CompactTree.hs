module Distribution.Client.Dependency.Modular.CompactTree  where

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.PSQ
import Control.Applicative
import Data.Function (on)
import Data.Foldable
import Data.Maybe
import Data.List (nubBy)
import Prelude hiding (foldr, mapM, lookup, foldr1)


doBFS :: Tree a -> Maybe (Path, IsDone)
doBFS = bfs.toCompact.toSimple

-- A normal tree without Stanza and Flag choices (for simplicity, for now)
data SimpleTree a =
    SPChoice     QPN a             (PSQ I        (SimpleTree a))
  | SGoalChoice                    (PSQ OpenGoal (SimpleTree a))
  | SFail        (ConflictSet QPN) FailReason
  | SDone
  deriving (Eq, Show)

toSimple :: Tree a -> SimpleTree a
toSimple (FChoice     _ _ _ _ t) = toSimple $ fromJust $ lookup False t
toSimple (SChoice     _ _ _   t) = toSimple $ fromJust $ lookup False t
toSimple (PChoice     q a     t) = SPChoice q a (toSimple <$> t)
toSimple (GoalChoice          t) = SGoalChoice $ sortPSQ (toSimple <$> t)
toSimple (Done        _        ) = SDone
toSimple (Fail        c f     _) = SFail c f


data CompactTree a =
    CGoalChoice  (PSQ OpenGoal (CompactTree a))
  | CFail        (ConflictSet QPN) FailReason
  | CDone
  deriving (Eq, Show)

toCompact :: SimpleTree a -> CompactTree a
toCompact (SGoalChoice ts)   = CGoalChoice (toCompact <$> ts)
toCompact (SPChoice _  _ ts) = foldr1 mergeTree (toCompact <$> ts)
toCompact (SFail c fr)       = CFail c fr
toCompact SDone              = CDone


mergeTree :: CompactTree a -> CompactTree a -> CompactTree a
mergeTree CDone             _                  = CDone
mergeTree _                 CDone              = CDone
mergeTree (CFail _ _)       x                  = x
mergeTree x                 (CFail _ _)        = x
mergeTree (CGoalChoice psq) (CGoalChoice psq') = CGoalChoice $ mergePSQ psq psq'


-- uhoh... check what this does..
mergePSQ :: PSQ OpenGoal (CompactTree a) -> PSQ OpenGoal (CompactTree a) -> PSQ OpenGoal (CompactTree a)
mergePSQ (PSQ []) x = x
mergePSQ x (PSQ []) = x
mergePSQ l@(PSQ ((c, t) : cs)) r@(PSQ ((d, u) : ds)) = case compare c d of
  LT -> cons c t $ mergePSQ (PSQ cs) r
  EQ -> cons c (mergeTree t u) $ mergePSQ (PSQ cs) (PSQ ds)
  GT -> cons d u $ mergePSQ l (PSQ ds)



type Path = [OpenGoal]
type IsDone = Bool

bfs :: CompactTree a -> Maybe (Path, IsDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsDone)]] -> Maybe (Path, IsDone)
    go [] = Nothing
    go ([] : xs) = go xs
    go ((x : _) : _) = Just x

-- finds the first Fail or Done node in the compacted tree
bfs' :: (Path -> Path) -> CompactTree a-> [[(Path, IsDone)]]
bfs' prefix CDone = [[(prefix [], True)]]
bfs' prefix (CFail _ _) = [[(prefix [], False)]]
bfs' prefix (CGoalChoice (PSQ cs)) = [] : zipConc ((\(x, t) -> bfs' ((x :) . prefix) t) <$> cs)

zipConc :: [[[(Path, IsDone)]]] -> [[(Path, IsDone)]]
zipConc = foldr conc []
  where
    conc :: [[(Path, IsDone)]] -> [[(Path, IsDone)]] -> [[(Path, IsDone)]]
    conc xs [] = xs
    conc [] xs = xs
    conc (x : xs) (y : ys) = (x ++ y) : conc xs ys


