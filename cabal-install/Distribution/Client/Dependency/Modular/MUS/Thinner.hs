module Distribution.Client.Dependency.Modular.MUS.Thinner where

import Distribution.Client.Dependency.Modular.MUS.CompactTree
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S

thinner :: CompactTree -> CompactTree
thinner tree = go (pathsInBFSOrder tree) [] tree
  where
    go :: [[Path]] -> Path -> CompactTree -> CompactTree
    go paths path (CGoalChoice psq) =
        CGoalChoice $ P.PSQ $ catMaybes $ map (\(goal, subTree) -> processSub paths path goal subTree) (P.toList psq)
    go _ _ x = x

    processSub :: [[Path]] -> Path -> COpenGoal -> CompactTree -> Maybe (COpenGoal, CompactTree)
    processSub paths path goal subTree | isFirst (goal:path) paths = Just (goal, go paths (goal:path) subTree)
                                       | otherwise                 = Nothing

isFirst :: Path -> [[Path]] -> Bool
isFirst path paths = firstFound == path
    where
      level = paths !! (length path)
      firstFound = fromMaybe (error "Thinner.isFirst: The impossible happened.. could not find path")
                             (findFirst path level)

findFirst :: Path -> [Path] -> Maybe Path
findFirst path paths = go paths
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
    go xs = (Prelude.map snd xs) : go subs
        where
          subs = Prelude.concat $ catMaybes $ Prelude.map makeSubs xs
          makeSubs (CGoalChoice psq, path) = Just $ Prelude.map ( \(goal, t) -> (t, goal : path)) $  P.toList psq
          makeSubs _                       = Nothing

