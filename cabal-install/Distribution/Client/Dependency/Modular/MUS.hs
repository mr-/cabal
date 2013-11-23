module Distribution.Client.Dependency.Modular.MUS  where

import qualified Data.List as L
import Prelude

import Control.Applicative ((<$>))
import Distribution.Client.Dependency.Modular.MUS.Thinner
import Distribution.Client.Dependency.Modular.MUS.CompactTree
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P

-- A Minimal Unsatisfiable Subset (MUS) is a set of constrained variables
-- (i.e. the packages we want to install) which cannot be set so that all
-- constraints are fulfilled.
-- The goal of this module is to find the smallest of those given the tree
-- generated by the modular solver.

-- That tree is basically an implication tree. Choosing a subtree corresponds
-- to making a choice (for a version of a package, or a flag, or a stanza).
-- That subtree contains further choices, but only those choices that are
-- compatible with the former choice.

-- Hence given an abstract MUS, walking the tree through the packages in
-- the MUS will immediatelly lead to a fail after the last package in
-- the MUS.

-- The idea to find those is to simply do a BFS in a suitable tree to
-- find the situation above.

-- For this, we collapse the tree, merging all the package-verson choice
-- so that finding a Fail-node in the tree is the same as finding a MUS.

-- Further, we try to make the tree thin enough to make the search feasible,
-- while keeping it wide enough to find MUSes.

-- Note that we do not really find MUSes, since every returned Path has to
-- include one of the packages given at the command line.
-- Also a package that is part of a MUS might have been a dependency of
-- another package way down the tree.
-- To determine whether a returned Path constitutes a MUS we would have to
-- check satisfiability after removing that package.

-- A shrinking would be needed to make it an actual MUS.

-- TODO: It is not quite clear why removeDuplicates is necessary..
-- but it works that way.

findMUS :: Tree a -> Maybe (Path, IsDone)
findMUS = bfs                 -- bfs for the first/shortest MUS or Done-Node
        . removeDuplicates    -- Remove duplicates..
        . thinner             -- Heuristics to make BFS more feasible
        . toCompact           -- Collapse the tree removing version choices
        . toSimple            -- Remove/Set flags. This could be brigher



-- bfs-search for a Fail or Done-node.
-- Returns the path to that node and indicates whether is Done or Fail.
bfs :: CompactTree -> Maybe (Path, IsDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsDone)]] -> Maybe (Path, IsDone)
    go []            = Nothing
    go ([] : xs)     = go xs
    go ((x : _) : _) = Just x


-- finds the first Fail or Done node in the compacted tree
bfs' :: (Path -> Path) -> CompactTree -> [[(Path, IsDone)]]
bfs' prefix CDone                    = [[(prefix [], True)]]
bfs' prefix (CFail _ _)              = [[(prefix [], False)]]
bfs' prefix (CGoalChoice (P.PSQ cs)) = [] : zipConc ((\(x, t) -> bfs' ((x :) . prefix) t) <$> cs)

zipConc :: [[[(Path, IsDone)]]] -> [[(Path, IsDone)]]
zipConc = foldr conc []
  where
    conc :: [[(Path, IsDone)]] -> [[(Path, IsDone)]] -> [[(Path, IsDone)]]
    conc xs []             = xs
    conc [] xs             = xs
    conc (x : xs) (y : ys) = (x ++ y) : conc xs ys


showPath :: Path -> String
showPath path = unwords $ L.intersperse "-" $ map showCOpenGoal path


-- Debugging output

showThinnedPaths :: Tree a -> String
showThinnedPaths x = unlines $ map (unwords . L.intersperse " - ") $
            map (map (show . map showCOpenGoal )) $ pathsInBFSOrder $ thinner $ toCompact $ toSimple x

showThinnedPathsBFS :: Tree a -> String
showThinnedPathsBFS = showbfs' . bfs' id . thinner . toCompact . toSimple

showbfs' :: [[(Path, IsDone)]] -> String
showbfs' x = unlines $ map (unwords . L.intersperse " - ") $
            map (map (\(path, isdone) -> show (map showCOpenGoal path) ++ " " ++ bool isdone "Done" "Fail")) x

bool :: Bool -> a -> a -> a
bool True x  _ = x
bool False _ y = y
