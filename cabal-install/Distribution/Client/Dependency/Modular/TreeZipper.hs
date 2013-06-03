module Distribution.Client.Dependency.Modular.TreeZipper where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM)

import Data.List (break)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P hiding (map)
import Distribution.Client.Dependency.Modular.Version
import Distribution.Client.Dependency.Modular.Tree


{-
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
-}


data Path a = 
    Top 
  | PChoicePoint (Path a) (PSQ I        (Tree a))   I       QPN a             (PSQ I        (Tree a))
  | FChoicePoint (Path a) (PSQ Bool     (Tree a))   Bool    QFN a Bool Bool   (PSQ Bool     (Tree a))
  | SChoicePoint (Path a) (PSQ Bool     (Tree a))   Bool    QSN a Bool        (PSQ Bool     (Tree a))
  | GChoicePoint (Path a) (PSQ OpenGoal (Tree a))   OpenGoal                  (PSQ OpenGoal (Tree a))


data Pointer a = Pointer { tree :: Tree a, context :: Path a}


fromTree :: Tree a -> Pointer a
fromTree t = Pointer t Top


focusUp :: Pointer a -> Maybe (Pointer a)
focusUp (Pointer t Top) = Nothing
focusUp (Pointer t (PChoicePoint path left index q a right )) = Just $ Pointer newTree newPath
  where newPath = path
        newTree = PChoice q a newPSQ
        newPSQ = left `pjoin` P.fromList [(index, t)] `pjoin` right
focusUp (Pointer t (FChoicePoint path left index q a b1 b2 right)) = Just $ Pointer newTree newPath
  where newPath = path
        newTree = FChoice q a b1 b2 newPSQ
        newPSQ = left `pjoin` P.fromList [(index, t)] `pjoin` right
focusUp (Pointer t (SChoicePoint path left index q a b right)) = Just $ Pointer newTree newPath
  where newPath = path
        newTree = SChoice q a b newPSQ
        newPSQ = left `pjoin` P.fromList [(index, t)] `pjoin` right
focusUp (Pointer t (GChoicePoint path left index  right)) = Just $ Pointer newTree newPath
  where newPath = path
        newTree = GoalChoice  newPSQ
        newPSQ = left `pjoin` P.fromList [(index, t)] `pjoin` right



focusChild :: ChildType -> Pointer a -> Maybe (Pointer a)
focusChild (CTP key) (Pointer oldTree@(PChoice q a psq) oldPath) = Just $ Pointer newTree newPath
  where Just newTree = P.lookup key psq
        newPath = PChoicePoint oldPath left key q a right
        (left, right) = psplitAt key psq

focusChild (CTF key) (Pointer oldTree@(FChoice q a b1 b2 psq) oldPath) = Just $ Pointer newTree newPath
  where Just newTree = P.lookup key psq
        newPath = FChoicePoint oldPath left key q a b1 b2 right
        (left, right) = psplitAt key psq

focusChild (CTS key) (Pointer oldTree@(SChoice q a b psq) oldPath) = Just $ Pointer newTree newPath
  where Just newTree = P.lookup key psq
        newPath = SChoicePoint oldPath left key q a b right
        (left, right) = psplitAt key psq

focusChild (CTOG key) (Pointer oldTree@(GoalChoice psq) oldPath) = Just $ Pointer newTree newPath
  where Just newTree = P.lookup key psq
        newPath = GChoicePoint oldPath left key right
        (left, right) = psplitAt key psq
focusChild _ _ = Nothing


data ChildType =
    CTP I 
  | CTF Bool
  | CTS Bool 
  | CTOG OpenGoal

children :: Pointer a -> Maybe ([ChildType])
children (Pointer (PChoice _ _ c) _)     = Just $ map CTP $ P.keys c
children (Pointer (FChoice _ _ _ _ c) _) = Just $ map CTF $ P.keys c 
children (Pointer (SChoice _ _ _ c) _)   = Just $ map CTS $ P.keys c
children (Pointer (GoalChoice c) _)      = Just $ map CTOG $ P.keys c
children _ = Nothing



-- That is going to PSQ.hs
pjoin :: PSQ k v -> PSQ k v -> PSQ k v
pjoin (PSQ a) (PSQ b) = PSQ (a ++ b)

psplitAt :: (Eq k) => k -> PSQ k v -> (PSQ k v, PSQ k v) --everything is left if key is not found
psplitAt key (PSQ l) = (PSQ left, PSQ right)
  where (left, _:right)  = break (\(k,_)-> k == key) l


{- 

{ -#LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds, ExistentialQuantification# - }

data X = S | I

type family LeafType (x :: X ) :: *
type instance LeafType S = String
type instance LeafType I = Int

type family NodeType (x :: X) (a :: *) :: *
type instance NodeType S a = [Tree S a]
type instance NodeType I a = [(Int,Tree I a)]

data Tree s a where 
 Node :: a -> NodeType s a -> Tree s a
 Leaf :: LeafType s -> Tree s a
 
data SomeTree a = forall s . SomeTree (Tree s a)


data NodeType a =
    PChoice     QPN a
  | FChoice     QFN a Bool Bool
  | SChoice     QSN a Bool


data LeafType = 
    GoalChoice
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show)


data ContainerType a =          -- This should be constrained..
    PSQ I a
  | PSQ Bool a
  | PSQ OpenGoal a


data Treee a = LeafType | Node (NodeType a) (ContainerType (Treee a))

data Path a = Top | Point (Path a) (ContainerType a) (NodeType a) (ContainerType a)
type Forest a = [Tree a]

data Tree a = Item a | Node a (Forest a)

data Path a = Top | Point (Path a) (Forest a) a (Forest a) 


data Pointer a = Pointer {tree :: Tree a, context :: Path a}


expr :: Tree String
expr = Node "+" [ Node "*" [ Item "a", Item "b"], 
    Node "*" [ Item "c", Item "d"] ]


fromTree :: Tree a -> Pointer a
fromTree t = Pointer t Top

focusChild :: Int -> Pointer a -> Maybe (Pointer a)
focusChild n (Pointer (Node a children) path) = case splitChildren [] children n of
    Nothing -> Nothing
    Just (l':ls',r') -> Just $ Pointer l' $ (Point path ls' a r')
    _ -> Nothing
focusChild _ _ = Nothing 

focusLeft :: Pointer a -> Maybe (Pointer a)
focusLeft c@(Pointer t p) = case p of
    Point path (l:ls) a r -> Just $ Pointer l (Point path ls a (t:r)) 
    _ -> Nothing

focusUp :: Pointer a -> Maybe (Pointer a)
focusUp (Pointer _ Top) = Nothing
focusUp (Pointer t (Point path l a r)) = Just $ Pointer (Node a (l ++ [t] ++ r)) path


splitChildren :: [a] -> [a] -> Int -> Maybe ([a],[a])
splitChildren acc xs 0      = Just (acc,xs)
splitChildren acc (x:xs) n  = splitChildren (x:acc) xs $! n-1
splitChildren _ _ _         = Nothing

main :: IO ()
main = do
    putStrLn $ drawTree $ tree $ fromJust $ focusChild 1 (fromTree expr) >>= focusChild 1 >>= focusUp >>= focusUp
    putStrLn $ drawTree $ tree $ fromJust $ focusUp =<< focusUp =<< focusChild 1 =<< focusChild 1 (fromTree expr) 





drawTree :: (Show a) => Tree a-> String
drawTree  = unlines . draw

draw :: (Show a) => Tree a -> [String]
draw (Item x) = [show x]
draw (Node x l) = show x : drawSubTrees l 
    where   drawSubTrees [] = []
            drawSubTrees [t] =
                "|" : shift "`- " "   " (draw t)
            drawSubTrees (t:ts) =
                "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
            shift first other = zipWith (++) (first : repeat other)
-}
