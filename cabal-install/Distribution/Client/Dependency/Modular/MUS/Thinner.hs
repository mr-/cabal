module Distribution.Client.Dependency.Modular.MUS.Thinner where

import Distribution.Client.Dependency.Modular.MUS.CompactTree
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Data.Maybe (catMaybes)
import qualified Data.Set as S

thinner :: CompactTree -> CompactTree
thinner tree = thinner' (pathsInBFSOrder tree) [] tree

thinner' :: [[Path]] -> Path -> CompactTree -> CompactTree
thinner' paths path (CGoalChoice psq) =
    CGoalChoice $ P.PSQ $ catMaybes $ map (\(goal, tree) -> processSub paths path goal tree) (P.toList psq)
thinner' _ _ x = x


processSub :: [[Path]] -> Path -> COpenGoal -> CompactTree -> Maybe (COpenGoal, CompactTree)
processSub paths path goal tree | isFirst (goal:path) paths = Just (goal, thinner' paths (goal:path) tree)
                                | otherwise                 = Nothing

isFirst :: Path -> [[Path]] -> Bool
isFirst path paths = findFirst path level == path
    where level = paths !! (length path)

findFirst :: Path -> [Path] -> Path
findFirst path paths = go paths
  where
    go (p:aths)
      | setPath == S.fromList p  =  p
      | otherwise                = go aths
    setPath = S.fromList path

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

