module Distribution.Client.Dependency.Modular.CompactTree  where

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Control.Applicative hiding (empty)
import Data.Foldable hiding (toList)
import Data.Maybe
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Set as S hiding (foldr)
import Prelude hiding (foldr, mapM, lookup, foldr1)


doBFS :: Tree a -> Maybe (Path, IsDone)
doBFS = bfs . thinner . toCompact . toSimple

-- A normal tree without Stanza and Flag choices (for simplicity, for now)
data SimpleTree a =
    SPChoice     QPN a             (P.PSQ I        (SimpleTree a))
  | SGoalChoice                    (P.PSQ OpenGoal (SimpleTree a))
  | SFail        (ConflictSet QPN) FailReason
  | SDone
  deriving (Eq, Show)

toSimple :: Tree a -> SimpleTree a
toSimple (FChoice     _ _ _ _ t) = toSimple $ fromJust $ P.lookup False t
toSimple (SChoice     _ _ _   t) = toSimple $ fromJust $ P.lookup False t
toSimple (PChoice     q a     t) = SPChoice q a (toSimple <$> t)
toSimple (GoalChoice          t) = SGoalChoice $ removeDuplicates $ P.sortPSQ (toSimple <$> t)
toSimple (Done        _        ) = SDone
toSimple (Fail        c f     _) = SFail c f

removeDuplicates :: (Ord a) => P.PSQ a v -> P.PSQ a v
removeDuplicates psq = P.fromList $ map (\k -> (k,fromJust $ P.lookup k psq)) $ L.nub $ P.keys psq

data CompactTree =
    CGoalChoice  (P.PSQ COpenGoal CompactTree)
  | CFail        (ConflictSet QPN) FailReason
  | CDone
  deriving (Show)

-- A wrapper to get the necessary Ord-instance
-- without jeopardizing Eq
data COpenGoal = COpenGoal OpenGoal

showCOpenGoal :: COpenGoal -> String
showCOpenGoal (COpenGoal goal) = showOpenGoal goal

instance Ord COpenGoal where
   compare = comparing showCOpenGoal

instance Eq COpenGoal where
  a == b = case a `compare` b of
            EQ -> True
            _  -> False

instance Show COpenGoal where
  show = showCOpenGoal


thinner :: CompactTree -> CompactTree
thinner tree = thinner' (pathsInBFSOrder tree) [] tree

thinner' :: [[Path]] -> Path -> CompactTree -> CompactTree
thinner' paths path (CGoalChoice psq) =
    CGoalChoice $ P.PSQ $ catMaybes $ map (\(goal, tree) -> processSub paths path goal tree) (P.toList psq)
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
      makeSubs (CGoalChoice psq, path) = Just $ Prelude.map ( \(goal, t) -> (t, goal : path)) $  P.toList psq
      makeSubs _                       = Nothing






showThinnedPaths :: Tree a -> String
showThinnedPaths x = unlines $ map unwords $ map (L.intersperse " - ") $
            map (map (\path -> (show (map showCOpenGoal path)))) $ pathsInBFSOrder $ thinner $ toCompact $ toSimple x

showPath :: Path -> String
showPath path = unwords $ L.intersperse " - " $ map showCOpenGoal path

baz :: Tree a -> String
baz x = show $ length d - length (L.nub d)
-- $ unwords $ map showPath $ d L.\\ (L.nub d)
  where d = (pathsInBFSOrder $ thinner $ toCompact $ toSimple x) !! 2




toCompact :: SimpleTree a -> CompactTree
toCompact (SGoalChoice ts)   = CGoalChoice $ P.mapKeys COpenGoal (toCompact <$> ts)
toCompact (SPChoice _  _ ts) = foldr1 mergeTree (toCompact <$> ts)
toCompact (SFail c fr)       = CFail c fr
toCompact SDone              = CDone


mergeTree :: CompactTree -> CompactTree -> CompactTree
mergeTree CDone             _                  = CDone
mergeTree _                 CDone              = CDone
mergeTree (CFail _ _)       x                  = x
mergeTree x                 (CFail _ _)        = x
mergeTree (CGoalChoice psq) (CGoalChoice psq') = CGoalChoice $ P.unionWith mergeTree psq psq'



type Path = [COpenGoal]
type IsDone = Bool

bfs :: CompactTree -> Maybe (Path, IsDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsDone)]] -> Maybe (Path, IsDone)
    go []            = Nothing
    go ([] : xs)     = go xs
    go ((x : _) : _) = Just x

showThinnedPathsBFS :: Tree a -> String
showThinnedPathsBFS = showbfs' . bfs' id . thinner . toCompact . toSimple

showbfs' :: [[(Path, IsDone)]] -> String
showbfs' x = unlines $ map unwords $ map (L.intersperse " - ") $
            map (map (\(path, isdone) -> (show (map showCOpenGoal path)) ++ " " ++ bool isdone "Done" "Fail")) x

bool :: Bool -> a -> a -> a
bool True x  _ = x
bool False _ y = y


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


