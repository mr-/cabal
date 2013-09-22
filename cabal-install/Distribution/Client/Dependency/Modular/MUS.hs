module Distribution.Client.Dependency.Modular.MUS  where

import qualified Data.List as L
import Prelude

import Control.Applicative ((<$>))
import Distribution.Client.Dependency.Modular.MUS.Thinner
import Distribution.Client.Dependency.Modular.MUS.CompactTree
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P


-- It is not quite clear why removeDuplicates is necessary..
-- but it works that way.

findMUS :: Tree a -> Maybe (Path, IsDone)
findMUS = bfs .                -- bfs for the first/shortest MUS
          removeDuplicates .    -- remove duplicates..
          thinner .             -- heuristics to make BFS more feasible
          toCompact . toSimple  -- Collapse tree



bfs :: CompactTree -> Maybe (Path, IsDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsDone)]] -> Maybe (Path, IsDone)
    go []            = Nothing
    go ([] : xs)     = go xs
    go ((x : _) : _) = Just x


-- finds the first Fail or Done node in the compacted tree
bfs' :: (Path -> Path) -> CompactTree -> [[(Path, IsDone)]]
bfs' prefix CDone                  = [[(prefix [], True)]]
bfs' prefix (CFail _ _)            = [[(prefix [], False)]]
bfs' prefix (CGoalChoice (P.PSQ cs)) = [] : zipConc ((\(x, t) -> bfs' ((x :) . prefix) t) <$> cs)

zipConc :: [[[(Path, IsDone)]]] -> [[(Path, IsDone)]]
zipConc = foldr conc []
  where
    conc :: [[(Path, IsDone)]] -> [[(Path, IsDone)]] -> [[(Path, IsDone)]]
    conc xs []             = xs
    conc [] xs             = xs
    conc (x : xs) (y : ys) = (x ++ y) : conc xs ys



-- Debugging output

showThinnedPaths :: Tree a -> String
showThinnedPaths x = unlines $ map unwords $ map (L.intersperse " - ") $
            map (map (\path -> (show (map showCOpenGoal path)))) $ pathsInBFSOrder $ thinner $ toCompact $ toSimple x

showThinnedPathsBFS :: Tree a -> String
showThinnedPathsBFS = showbfs' . bfs' id . thinner . toCompact . toSimple

showbfs' :: [[(Path, IsDone)]] -> String
showbfs' x = unlines $ map unwords $ map (L.intersperse " - ") $
            map (map (\(path, isdone) -> (show (map showCOpenGoal path)) ++ " " ++ bool isdone "Done" "Fail")) x

bool :: Bool -> a -> a -> a
bool True x  _ = x
bool False _ y = y

showPath :: Path -> String
showPath path = unwords $ L.intersperse " - " $ map showCOpenGoal path
