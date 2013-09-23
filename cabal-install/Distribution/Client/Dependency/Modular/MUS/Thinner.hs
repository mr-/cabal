module Distribution.Client.Dependency.Modular.MUS.Thinner where

import Distribution.Client.Dependency.Modular.MUS.CompactTree
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S


-- Given a compacted tree, we make it thinner by removing nodes that don't lead to new
-- unordered paths. That is wider than P.firstChoice but retains the possibility to find
-- smaller MUSes.
-- For example, given the tree  * - foo - bar
--                              \_ bar - foo
-- the thinned tree will be     * - foo - bar
--                              \_ bar
-- removing the foo after bar

thinner :: CompactTree -> CompactTree
thinner tree = go (map uniquePaths $ pathsInBFSOrder tree) [] tree
  where
    go :: [[Path]] -> Path -> CompactTree -> CompactTree
    go paths path (CGoalChoice psq) =
        CGoalChoice $ P.PSQ $ catMaybes $ map ( uncurry (processSub paths path) ) (P.toList psq)
    go _ _ x = x

    processSub :: [[Path]] -> Path -> COpenGoal -> CompactTree -> Maybe (COpenGoal, CompactTree)
    processSub paths path goal subTree | isFirst (goal:path) paths = Just (goal, go paths (goal:path) subTree)
                                       | otherwise                 = Nothing

-- checks if the given path is the first in its level
isFirst :: Path -> [[Path]] -> Bool
isFirst path paths = firstFound == path
    where
      level = paths !! length path
      firstFound = fromMaybe (error "Thinner.isFirst: The impossible happened.. could not find path")
                             (findFirst path level)

-- Find the first occurence of the path.
-- Paths are considered unordered.
findFirst :: Path -> [Path] -> Maybe Path
findFirst path = go
  where
    setPath = S.fromList path
    go :: [Path] -> Maybe Path
    go []                        = Nothing
    go (p:aths)
      | setPath == S.fromList p  = Just p
      | otherwise                = go aths


pathsInBFSOrder :: CompactTree -> [[Path]]
pathsInBFSOrder tree = go [(tree, [])]
  where
    go :: [(CompactTree, Path)] -> [[Path]]
    go [] = []
    go xs = map snd xs : go subs
        where
          subs = Prelude.concat $ mapMaybe makeSubs xs
          makeSubs (CGoalChoice psq, path) = Just $ Prelude.map ( \(goal, t) -> (t, goal : path)) $  P.toList psq
          makeSubs _                       = Nothing


type UnorderedPath = S.Set COpenGoal

uniquePaths :: [Path] -> [Path]
uniquePaths = catMaybes . go S.empty
  where
    go :: S.Set (UnorderedPath) -> [Path] -> [Maybe Path]
    go _          []                            = []
    go setOfPaths (path:xs)
      | (S.fromList path) `S.member` setOfPaths = Nothing   : go setOfPaths xs
      | otherwise                               = Just path : go (S.insert (S.fromList path) setOfPaths) xs

