module Distribution.Client.Dependency.Modular.CompactTree  where

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.PSQ
import Control.Applicative hiding (empty)
import Control.Monad.State.Lazy
import Data.Foldable hiding (toList)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Set as S hiding (foldr)
import Prelude hiding (foldr, mapM, lookup, foldr1)


doBFS :: Tree a -> Maybe (Path, IsDone)
doBFS = bfs . thinner . toCompact . toSimple

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



-- type UnorderedPath = S.Set COpenGoal
-- type ThinState = M.Map Int (S.Set UnorderedPath)
--
-- thinner :: CompactTree -> CompactTree
-- thinner tree = evalState (thinner' S.empty tree) M.empty
--
-- thinner' :: UnorderedPath -> CompactTree -> State ThinState CompactTree
-- thinner' path (CGoalChoice psq) = do
--     stuff <- mapM (\(k,v) -> processSubTree path k v) (toList psq)
--     return $ CGoalChoice $ PSQ (catMaybes stuff)
-- thinner' _    x                 = return x
--
-- processSubTree :: UnorderedPath -> COpenGoal -> CompactTree -> State ThinState (Maybe (COpenGoal, CompactTree))
-- processSubTree path goal tree = do
--   paths <- get
--   let newPath   = S.insert goal path
--       level     = S.size newPath
--   case newPath `S.member` fromMaybe S.empty (M.lookup level paths) of
--     True  -> return Nothing
--     False -> do
--             put $  M.insertWith S.union level (S.singleton newPath) paths
--             newTree <- thinner' newPath tree
--             return (Just (goal, newTree))


type UnorderedPath = S.Set COpenGoal
type ThinState = [(S.Set UnorderedPath)]

thinner :: CompactTree -> CompactTree
thinner tree = evalState (thinner' S.empty tree) []

thinner' :: UnorderedPath -> CompactTree -> State ThinState CompactTree
thinner' path (CGoalChoice psq) = do
    stuff <- mapM (\(k,v) -> processSubTree path k v) (toList psq)
    return $ CGoalChoice $ PSQ (catMaybes stuff)
thinner' _ x = return x

processSubTree :: UnorderedPath -> COpenGoal -> CompactTree -> State ThinState (Maybe (COpenGoal, CompactTree))
processSubTree path goal tree = do
  paths <- get
  let newPath   = S.insert goal path
  case checkPathExists newPath paths of
    True  -> return Nothing
    False -> do
            put $ addPath newPath paths
            newCompactTree <- thinner' newPath tree
            return (Just (goal, newCompactTree))

checkPathExists :: (Ord a) => S.Set a -> [S.Set (S.Set a)] -> Bool
checkPathExists path paths = S.member path level
    where level = fromMaybe S.empty $ saveGet (S.size path) paths

saveGet :: Int -> [a] -> Maybe a
saveGet i l = go i l
    where
        go 0 (x:_)  = Just x
        go n (_:xs) = go (n-1) xs
        go _ []     = Nothing

addPath :: (Ord a) => S.Set a -> [S.Set (S.Set a)] -> [S.Set (S.Set a)]
addPath path paths = go (S.size path) paths
    where
        go 0 (p:aths) = S.insert path p : aths
        go n (p:aths) = p : go (n-1) aths
        go 0 []       = [S.singleton path]
        go n []       = S.empty : go (n-1) []





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
mergeTree (CGoalChoice psq) (CGoalChoice psq') = CGoalChoice $ unionWith mergeTree psq psq'



type Path = [COpenGoal]
type IsDone = Bool

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
bfs' prefix (CGoalChoice (PSQ cs)) = [] : zipConc ((\(x, t) -> bfs' ((x :) . prefix) t) <$> cs)

zipConc :: [[[(Path, IsDone)]]] -> [[(Path, IsDone)]]
zipConc = foldr conc []
  where
    conc :: [[(Path, IsDone)]] -> [[(Path, IsDone)]] -> [[(Path, IsDone)]]
    conc xs []             = xs
    conc [] xs             = xs
    conc (x : xs) (y : ys) = (x ++ y) : conc xs ys


