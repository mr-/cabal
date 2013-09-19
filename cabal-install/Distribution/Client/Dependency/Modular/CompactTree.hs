 {-# LANGUAGE TupleSections #-}
module Distribution.Client.Dependency.Modular.CompactTree  where

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.PSQ
import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Foldable hiding (toList)
import Data.Maybe
import Data.Ord (comparing)
import Data.Set hiding (foldr, toList)
import Prelude hiding (foldr, mapM, lookup, foldr1)


doBFS :: Tree a -> Maybe (Path, IsDone)
doBFS = bfs.thinner.toCompact.toSimple

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


data CompactTree =
    CGoalChoice  (PSQ COpenGoal CompactTree)
  | CFail        (ConflictSet QPN) FailReason
  | CDone
  deriving (Show)

-- A wrapper to get the necessary Ord-instance
-- without jeopardizing Eq
data COpenGoal = COpenGoal OpenGoal
  deriving (Show)

showCOpenGoal :: COpenGoal -> String
showCOpenGoal (COpenGoal goal) = showOpenGoal goal

instance Ord COpenGoal where
   compare = comparing showCOpenGoal

instance Eq COpenGoal where
  a == b = case a `compare` b of
            EQ -> True
            _  -> False


type UnorderedPath = Set COpenGoal

thinner :: CompactTree -> CompactTree
thinner tree = evalState (thinner' tree)  (TS empty empty)

data ThinState = TS { allPaths :: Set UnorderedPath
                    , toHere   :: UnorderedPath     }

-- this should do BFS, or else it has to calculate the whole Tree
-- to to do the second element in the bfs..

thinner' :: CompactTree -> State ThinState CompactTree
thinner' (CGoalChoice psq) = do
    stuff <- mapM (\(k,v) -> doSmth k v) (toList psq)
    return $ CGoalChoice $ PSQ (catMaybes stuff)
thinner' x = return x

doSmth :: COpenGoal -> CompactTree -> State ThinState (Maybe (COpenGoal, CompactTree))
doSmth goal tree = do
  (TS paths path) <- get
  let newPath   = insert goal path
  case newPath `member` paths of
    True  -> return Nothing
    False -> do
            put (TS (insert newPath paths) newPath)
            newTree <- thinner' tree
            return (Just (goal, newTree))

-- foldrM :: (a -> b -> m b) -> b -> t a -> m b
--Data.Foldable.foldr1 :: Foldable t => (a -> a -> a) -> t a -> a

toCompact :: SimpleTree a -> CompactTree
toCompact (SGoalChoice ts)   = CGoalChoice $ mapKeys COpenGoal (toCompact <$> ts)
toCompact (SPChoice _  _ ts) = foldr1 mergeTree (toCompact <$> ts)
toCompact (SFail c fr)       = CFail c fr
toCompact SDone              = CDone


mergeTree :: CompactTree -> CompactTree -> CompactTree
mergeTree CDone             _                  = CDone
mergeTree _                 CDone              = CDone
mergeTree (CFail _ _)       x                  = x
mergeTree x                 (CFail _ _)        = x
mergeTree (CGoalChoice psq) (CGoalChoice psq') = CGoalChoice $ mergePSQ psq psq'


mergePSQ :: PSQ COpenGoal CompactTree -> PSQ COpenGoal CompactTree -> PSQ COpenGoal CompactTree
mergePSQ = unionWith mergeTree


type Path = [COpenGoal]
type IsDone = Bool

bfs :: CompactTree -> Maybe (Path, IsDone)
bfs t = go (bfs' id t)
  where
    go :: [[(Path, IsDone)]] -> Maybe (Path, IsDone)
    go [] = Nothing
    go ([] : xs) = go xs
    go ((x : _) : _) = Just x

-- finds the first Fail or Done node in the compacted tree
bfs' :: (Path -> Path) -> CompactTree -> [[(Path, IsDone)]]
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


