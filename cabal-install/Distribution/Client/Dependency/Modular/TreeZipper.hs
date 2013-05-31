module Distribution.Client.Dependency.Modular.TreeZipper where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Version


data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show)
  -- Above, a choice is called trivial if it clearly does not matter. The
  -- special case of triviality we actually consider is if there are no new
  -- dependencies introduced by this node.

data NodeType a =
    PChoice     QPN a
  | FChoice     QFN a Bool Bool
  | SChoice     QSN a Bool


data NodeContainer a = A a [a] | B a [(a,a)]

data LeafType = 
    GoalChoice
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show)


data ContainerType a =          -- This should be constrained..
    PSQ I a
  | PSQ Bool a
  | PSQ OpenGoal a




{-#LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds, ExistentialQuantification#-}

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




data Treee a = LeafType | Node (NodeType a) (ContainerType (Treee a))

data Path a = Top | Point (Path a) (ContainerType a) (NodeType a) (ContainerType a)

data Pointer a = Pointer { tree :: Treee a, context :: Path a}


converse :: Tree a -> Treee a
converse = undefined



{- 

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
