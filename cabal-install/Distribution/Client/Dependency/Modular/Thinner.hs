module Distribution.Client.Dependency.Modular.Thinner where

import qualified Data.Set as S
import Control.Monad.State
import Control.Monad
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Data.Maybe
import qualified Distribution.Client.Dependency.Modular.PSQ as P

--data Tree a =
--    PChoice     QPN a           (PSQ I        (Tree a))
--  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
--  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
--  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
--  | Done        RevDepMap
--  | Fail        (ConflictSet QPN) FailReason (Maybe (Tree a))
--  deriving (Eq, Show)
--
type SetPath = S.Set (Var QPN, ChildType)
data Bucket = B {allV :: S.Set SetPath } deriving (Eq, Ord)

--data Bucket = B {allV :: S.Set (S.Set ChildType)} deriving (Eq, Ord)
--
--for now ignore flags and stanzas?
thinner :: Tree a -> SetPath -> State Bucket (Tree a)
thinner (PChoice    a b     st) toHere = do
    allVisited <- gets allV
    let assocs     = P.toList st
        keys       = map fst assocs
        realHere   = (P a, CTP x) `S.insert` toHere -- this does not work yet...
        unvisited  = filter (\x -> S.member realHere  allVisited) keys
        subTree ch = fromJust $ P.lookup ch st
--    modify (S.insert toHere)
--    foo <- mapM (thinner $ fromJust (P.lookup unv st)) unvisited
    subTrees <- mapM (\child -> thinner (subTree child) realHere) unvisited
    return $ PChoice a b $ P.fromList $ zip unvisited subTrees
--    join foo and update state.

thinner (FChoice    _ _ _ _ _) _ = undefined
thinner (SChoice    _ _ _   _) _ = undefined
thinner (GoalChoice         _) _ = undefined
thinner (Done       _        ) _ = undefined
thinner (Fail       _ _     _) _ = undefined
