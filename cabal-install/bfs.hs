import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative ((<$>))
import Data.Maybe
import Control.Monad.State




data CompactTree = CGoalChoice [(String, CompactTree)] -- assume goal choices are globally sorted in this list
          | COtherChoice [CompactTree]          -- the labels of the other choices don't matter here, so we omit them; we assume non-empty choices
          | CDone
          | CFail
          | CUndecided                   -- represents a tree that could either succeed or fail, without going into details
  deriving (Show)

-- We proceed in two phases. First, we collapse a tree into a tree that only
-- contains the goal choices. If, after a goal choice, all subsequent other
-- choices yield CFail nodes, we store a single CFail node instead. Similarly, if
-- we have at least one CDone node, we store a single CDone node.
--
-- This process is a normal top-down traversal over the tree and doesn't affect
-- our ability to do bfs later.
main = undefined
-- bfcompact removes all COtherChoice nodes; we could use a different datatype for the resulting tree to make that clear
bfcompact :: CompactTree -> CompactTree
bfcompact (CGoalChoice cs)  = CGoalChoice (map (\ (x, t) -> (x, bfcompact t)) cs)
bfcompact (COtherChoice ts) = foldr1 mergeCompactTree (map bfcompact ts)
bfcompact CDone             = CDone
bfcompact CFail             = CFail
bfcompact CUndecided        = CUndecided

-- merges two trees without COtherChoice nodes
mergeCompactTree :: CompactTree -> CompactTree -> CompactTree
mergeCompactTree CDone _           = CDone
mergeCompactTree _ CDone           = CDone
mergeCompactTree CFail x           = x
mergeCompactTree x CFail           = x
mergeCompactTree CUndecided x      = x
mergeCompactTree x CUndecided      = x
mergeCompactTree (CGoalChoice cs) (CGoalChoice ds) = CGoalChoice (mergeList cs ds)

mergeList :: [(String, CompactTree)] -> [(String, CompactTree)] -> [(String, CompactTree)]
mergeList cs [] = cs
mergeList [] ds = ds
mergeList ((c, t) : cs) ((d, u) : ds) = case compare c d of
  LT -> (c, t) : mergeList cs ((d, u) : ds)
  EQ -> (c, mergeCompactTree t u) : mergeList cs ds
  GT -> (d, u) : mergeList ((c, t) : cs) ds

-- In the second phase, we perform a "normal" bfs to find the first CFail or
-- CDone node (in bfs) order. The trick is to use a helper-function bfs' that
-- returns a lazy list of list of nodes, one per "level" of the original tree.
--
-- It's probably possible and perhaps even a good idea to fuse the two phases.

type Path = [COpenGoal]
type IsCDone = Bool

bfs :: CompactTree -> Maybe (Path, IsCDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsCDone)]] -> Maybe (Path, IsCDone)
    go [] = Nothing
    go ([] : xs) = go xs
    go ((x : _) : _) = Just x

-- finds the first CFail or CDone node in the compacted tree
bfs' :: (Path -> Path) -> CompactTree -> [[(Path, IsCDone)]]
bfs' prefix CDone = [[(prefix [], True)]]
bfs' prefix CFail = [[(prefix [], False)]]
bfs' prefix CUndecided = []
bfs' prefix (CGoalChoice cs) = [] : zipConc (map (\ (x, t) -> bfs' ((x :) . prefix) t) cs)

zipConc :: [[[(Path, IsCDone)]]] -> [[(Path, IsCDone)]]
zipConc = foldr conc []
  where
    conc :: [[(Path, IsCDone)]] -> [[(Path, IsCDone)]] -> [[(Path, IsCDone)]]
    conc xs [] = xs
    conc [] xs = xs
    conc (x : xs) (y : ys) = (x ++ y) : conc xs ys

example :: CompactTree
example = CGoalChoice [("A", CGoalChoice [("B", CGoalChoice [("C", CFail)])])
                     ,("B", CGoalChoice [("A", CFail)] )
                     ,("C", CGoalChoice [("A", CGoalChoice [("B", CFail)])])
                     ]



type COpenGoal = String



thinner :: CompactTree -> CompactTree
thinner tree = thinner' (pathsInBFSOrder tree) [] tree

thinner' :: [[Path]] -> Path -> CompactTree -> CompactTree
thinner' paths path (CGoalChoice psq) =
    CGoalChoice $ catMaybes $ map (\(goal, tree) -> processSub paths path goal tree) psq
thinner' _ _ x = x


processSub :: [[Path]] -> Path -> COpenGoal -> CompactTree -> Maybe (COpenGoal, CompactTree)
processSub paths path goal tree | isFirst (goal:path) paths = Just (goal, thinner' paths (goal:path) tree)
                                | otherwise          = Nothing

isFirst :: Path -> [[Path]] -> Bool
isFirst path paths = findFirst path level == path
    where level = paths !! (length path)

findFirst :: Path -> [Path] -> Path
findFirst path (p:aths)
    | S.fromList path == S.fromList p  =  p
    | otherwise                        = findFirst path aths

pathsInBFSOrder :: CompactTree -> [[Path]]
pathsInBFSOrder tree = foo' [(tree, [])]

foo' :: [(CompactTree, Path)] -> [[Path]]
foo' [] = []
foo' xs = (Prelude.map snd xs) : foo' subs
    where
      subs = Prelude.concat $ catMaybes $ Prelude.map makeSubs xs
      makeSubs (CGoalChoice psq, path) = Just $ Prelude.map ( \(goal, t) -> (t, goal : path)) $  psq
      makeSubs _                       = Nothing

