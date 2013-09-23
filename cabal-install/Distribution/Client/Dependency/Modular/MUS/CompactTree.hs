module Distribution.Client.Dependency.Modular.MUS.CompactTree  where

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Control.Applicative hiding (empty)
import Data.Foldable hiding (toList, or)
import Data.Maybe
import Data.Ord (comparing)
import Prelude hiding (foldr1)


type Path = [COpenGoal]
type IsDone = Bool

-- A wrapper to get the necessary Ord-instance
-- without jeopardizing Eq
data COpenGoal = COpenGoal OpenGoal

-- A normal tree without Stanza and Flag choices (for simplicity, for now)
data SimpleTree a =
    SPChoice     QPN a             (P.PSQ I        (SimpleTree a))
  | SGoalChoice                    (P.PSQ OpenGoal (SimpleTree a))
  | SFail        (ConflictSet QPN) FailReason
  | SDone
  deriving (Eq, Show)


-- In this tree, a CFail is a pseudo-MUS.
data CompactTree =
    CGoalChoice  (P.PSQ COpenGoal CompactTree)
  | CFail        (ConflictSet QPN) FailReason
  | CDone
  deriving (Show)

instance Ord COpenGoal where
   compare = comparing showCOpenGoal

instance Eq COpenGoal where
  a == b = case a `compare` b of
            EQ -> True
            _  -> False

instance Show COpenGoal where
  show = showCOpenGoal


showCOpenGoal :: COpenGoal -> String
showCOpenGoal (COpenGoal goal) = showOpenGoal goal

-- For now, we eliminate flag and stanza-choices by setting them False.
-- Hence introducing fewer dependencies. This might not matter too much,
-- as we look for small MUSes mostly.
-- TODO: Better choose them according to the default.
toSimple :: Tree a -> SimpleTree a
toSimple (FChoice     _ _ _ _ t) = toSimple $ fromJust $ P.lookup False t
toSimple (SChoice     _ _ _   t) = toSimple $ fromJust $ P.lookup False t
toSimple (PChoice     q a     t) = SPChoice q a (toSimple <$> t)
toSimple (GoalChoice          t) = SGoalChoice  (toSimple <$> t)
toSimple (Done        _        ) = SDone
toSimple (Fail        c f     _) = SFail c f


-- TODO: There is a bug that introduces duplicates in the PSQs.
-- hence removeDuplicates.

-- toCompact collapses version-choices. If all of them are Failing, we have found a (pseudo-)MUS.
-- If one is Done, the problem admits a solution and we are done.
-- If we are left with Package-choices, we collect all of them.
toCompact :: SimpleTree a -> CompactTree
toCompact (SGoalChoice ts)   = CGoalChoice $ P.sortPSQ $ P.mapKeys COpenGoal (toCompact <$> ts)
toCompact (SPChoice _  _ ts) = foldr1 mergeTree (toCompact <$> ts)
toCompact (SFail c fr)       = CFail c fr
toCompact SDone              = CDone


mergeTree :: CompactTree -> CompactTree -> CompactTree
mergeTree CDone             _                  = CDone
mergeTree _                 CDone              = CDone
mergeTree (CFail _ _)       x                  = x
mergeTree x                 (CFail _ _)        = x
mergeTree (CGoalChoice psq) (CGoalChoice psq') = CGoalChoice $ P.unionWith mergeTree psq psq'


removeDuplicates :: CompactTree -> CompactTree
removeDuplicates (CGoalChoice psq) = CGoalChoice $ removeDuplicates <$> newPSQ
  where newPSQ = P.mergeDuplicatesWith mergeTree psq
removeDuplicates x = x





-- For debugging purposes
-- dup :: Ord a => [a] -> Maybe a
-- dup l = dup' l S.empty
--   where dup' [] _ = Nothing
--         dup' (x:xs) s = if S.member x s
--                            then Just x
--                            else dup' xs (S.insert x s)
--
-- psqHasDuplicates :: Ord a => P.PSQ a b -> Bool
-- psqHasDuplicates (P.PSQ psq) = isJust $ dup $ map fst psq
--
--
-- treeHasDuplicates :: Int -> CompactTree -> Bool
-- treeHasDuplicates _ (CFail _ _)               = False
-- treeHasDuplicates _ CDone                     = False
-- treeHasDuplicates 0 (CGoalChoice psq) = psqHasDuplicates psq
-- treeHasDuplicates n (CGoalChoice psq) = psqHasDuplicates psq || or (map (treeHasDuplicates (n - 1)) (P.values psq))


-- baz :: Tree a -> String
-- baz x = show $ length d - length (L.nub d)
-- -- $ unwords $ map showPath $ d L.\\ (L.nub d)
--   where d = (pathsInBFSOrder $ thinner $ toCompact $ toSimple x) !! 2
--
--
