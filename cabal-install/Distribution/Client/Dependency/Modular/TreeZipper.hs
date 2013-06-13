module Distribution.Client.Dependency.Modular.TreeZipper where

import Control.Applicative
import Control.Monad hiding (mapM)
import Prelude hiding (foldr, mapM)
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P hiding (map)
import Distribution.Client.Dependency.Modular.Tree

import Data.Maybe (fromJust)
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
  | PChoicePoint (Path a) (PSQContext I (Tree a))           QPN a
  | FChoicePoint (Path a) (PSQContext Bool (Tree a))        QFN a Bool Bool
  | SChoicePoint (Path a) (PSQContext Bool (Tree a))        QSN a Bool
  | GChoicePoint (Path a) (PSQContext OpenGoal (Tree a))


data Pointer a = Pointer { toPath :: Path a, toTree :: Tree a }

type WrongWayPath = [ChildType]
type OneWayPath   = [ChildType]

wrongToOne :: WrongWayPath -> OneWayPath
wrongToOne = reverse


pathToList :: Path a -> WrongWayPath
pathToList Top = []
pathToList (PChoicePoint path context _ _     ) = CTP  (P.contextKey context) : pathToList path
pathToList (FChoicePoint path context _ _ _ _ ) = CTF  (P.contextKey context) : pathToList path
pathToList (SChoicePoint path context _ _ _   ) = CTS  (P.contextKey context) : pathToList path
pathToList (GChoicePoint path context         ) = CTOG (P.contextKey context) : pathToList path


walk :: OneWayPath -> Pointer a -> Maybe (Pointer a)
walk []     _           = Nothing
walk [x]    treePointer = focusChild x treePointer
walk (x:xs) treePointer = focusChild x treePointer >>= walk xs


fromTree :: Tree a -> Pointer a
fromTree t = Pointer Top t

isRoot :: Pointer a -> Bool
isRoot (Pointer Top _ ) = True
isRoot _                = False

toTop :: Pointer a -> [Pointer a]
toTop = toTop'
  where
    toTop' :: Pointer a -> [Pointer a]
    toTop' pointer | isRoot pointer = []
    toTop' pointer = pointer : (toTop $ fromJust $ focusUp pointer)

focusUp :: Pointer a -> Maybe (Pointer a)
focusUp (Pointer Top _) = Nothing

focusUp (Pointer (PChoicePoint path context q a ) t) = Just $ Pointer path newTree
  where newTree = PChoice q a newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (FChoicePoint path context q a b1 b2 ) t) = Just $ Pointer path newTree
  where newTree = FChoice q a b1 b2 newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (SChoicePoint path context q a b ) t) = Just $ Pointer path newTree
  where newTree = SChoice q a b newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (GChoicePoint path context) t) = Just $ Pointer path newTree
  where newTree = GoalChoice  newPSQ
        newPSQ  = P.joinContext t context



data ChildType = CTP I
               | CTF Bool
               | CTS Bool
               | CTOG OpenGoal    deriving (Show)



focusChild :: ChildType -> Pointer a -> Maybe (Pointer a)
focusChild (CTP key)  (Pointer oldPath (PChoice q a psq))        = Pointer newPath <$> P.lookup key psq
  where newPath = PChoicePoint oldPath newContext q a
        newContext = P.makeContextAt key psq


focusChild (CTF key)  (Pointer oldPath (FChoice q a b1 b2 psq))  = Pointer newPath <$> P.lookup key psq
  where newPath = FChoicePoint oldPath newContext q a b1 b2
        newContext = P.makeContextAt key psq


focusChild (CTS key)  (Pointer oldPath (SChoice q a b psq))      = Pointer newPath <$> P.lookup key psq
  where newPath = SChoicePoint oldPath newContext q a b
        newContext = P.makeContextAt key psq


focusChild (CTOG key) (Pointer oldPath (GoalChoice psq))         = Pointer newPath <$> P.lookup key psq
  where newPath = GChoicePoint oldPath newContext
        newContext = P.makeContextAt key psq

focusChild _ _ = Nothing

focusRoot :: Pointer a -> Pointer a
focusRoot treePointer = maybe treePointer focusRoot (focusUp treePointer)


children :: Pointer a -> Maybe [ChildType]
children (Pointer _ (PChoice _ _     c)) = Just $ map CTP  $ P.keys c
children (Pointer _ (FChoice _ _ _ _ c)) = Just $ map CTF  $ P.keys c
children (Pointer _ (SChoice _ _ _   c)) = Just $ map CTS  $ P.keys c
children (Pointer _ (GoalChoice      c)) = Just $ map CTOG $ P.keys c
children _                               = Nothing





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
-}
