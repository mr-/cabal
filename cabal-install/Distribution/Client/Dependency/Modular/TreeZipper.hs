module Distribution.Client.Dependency.Modular.TreeZipper where

import Control.Applicative
import Control.Monad                                     hiding (mapM)
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ        as P hiding (map, filter)
import Distribution.Client.Dependency.Modular.Tree
import Prelude                                           hiding (foldr, mapM)

import Data.List                                         (isPrefixOf)
import Data.Maybe                                        (fromJust, fromMaybe, isNothing, catMaybes)

{-
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason (Maybe (Tree a))
-}



data Path a =
    Top
  | PChoicePoint (Path a) (PSQContext I        (Tree a)) QPN a
  | FChoicePoint (Path a) (PSQContext Bool     (Tree a)) QFN a Bool Bool
  | SChoicePoint (Path a) (PSQContext Bool     (Tree a)) QSN a Bool
  | GChoicePoint (Path a) (PSQContext OpenGoal (Tree a))
  | FailPoint    (Path a)                                (ConflictSet QPN) FailReason

data Pointer a = Pointer { toPath :: Path a, toTree :: Tree a }

--data PointerPair a = Pair { pairShallow :: Pointer a,  pairDeep :: Pointer a}

instance Functor Pointer where
  fmap f = liftToPtr (fmap f)


liftToPtr :: (Tree a -> Tree b) -> Pointer a -> Pointer b
liftToPtr f ptr = fromJust $ walk trail $ fromTree transRoot
  where
    transRoot = (f . toTree . focusRoot) ptr
    trail = (wrongToOne.pathToTrail.toPath) ptr


modifyTree :: (Tree a -> Tree a) -> Pointer a -> Pointer a
modifyTree f ptr = ptr { toTree = f (toTree ptr) }

type WrongWayTrail = [ChildType]
type OneWayTrail   = [ChildType]

wrongToOne :: WrongWayTrail -> OneWayTrail
wrongToOne = reverse

pathToTrail :: Path a -> WrongWayTrail
pathToTrail p = case pathToChild p of
  Nothing -> []
  Just ch -> ch : pathToTrail (fromJust $ innerPath p) -- it has an inner path, because it had ch

pathToChild :: Path a -> Maybe ChildType
pathToChild Top = Nothing
pathToChild (PChoicePoint _ context _ _     ) = Just $ CTP  (P.contextKey context)
pathToChild (FChoicePoint _ context _ _ _ _ ) = Just $ CTF  (P.contextKey context)
pathToChild (SChoicePoint _ context _ _ _   ) = Just $ CTS  (P.contextKey context)
pathToChild (GChoicePoint _ context         ) = Just $ CTOG (P.contextKey context)
pathToChild (FailPoint    _ _ _             ) = Just   CTFail

innerPath :: Path a -> Maybe (Path a)
innerPath Top = Nothing
innerPath (PChoicePoint path _ _ _     ) = Just path
innerPath (FChoicePoint path _ _ _ _ _ ) = Just path
innerPath (SChoicePoint path _ _ _ _   ) = Just path
innerPath (GChoicePoint path _         ) = Just path
innerPath (FailPoint    path _ _       ) = Just path

walk :: OneWayTrail -> Pointer a -> Maybe (Pointer a)
walk []     treePointer = Just treePointer
walk [x]    treePointer = focusChild x treePointer
walk (x:xs) treePointer = focusChild x treePointer >>= walk xs

pointsBelow :: Pointer a -> Pointer a -> Bool
a `pointsBelow` b = trail b `isPrefixOf` trail a
  where
    trail = wrongToOne.pathToTrail.toPath

intermediates :: Pointer a -> Pointer a -> [Pointer a]
intermediates shallow deep | shallow `pointsToSame` deep = []
intermediates shallow deep = deep : intermediates  shallow oneUp
  where oneUp = fromMaybe (error "Internal error: Provided malformed Pair") (focusUp deep)

intermediateTrail :: Pointer a -> Pointer a -> WrongWayTrail
intermediateTrail shallow deep | shallow `pointsToSame` deep = []
intermediateTrail shallow deep = (fromJust $ pathToChild $ toPath deep) : intermediateTrail shallow oneUp
  where oneUp = fromMaybe (error "Internal error: Provided malformed Pair") (focusUp deep)


filterBetween :: (Pointer a -> Bool) -> Pointer a -> Pointer a -> [Pointer a]
filterBetween pre nearPtr farPtr = filterUpTill (pointsToSame nearPtr) pre farPtr

filterUp :: (Pointer a -> Bool) -> Pointer a -> [Pointer a]
filterUp = filterUpTill (const False)

filterUpTill :: (Pointer a -> Bool) -> (Pointer a -> Bool) -> Pointer a -> [Pointer a]
filterUpTill stop _   ptr | stop ptr = []
filterUpTill stop pre ptr            = [ptr | pre ptr] ++ oneUp
  where oneUp = maybe [] (filterUpTill stop pre) (focusUp ptr)

pointsToSame :: Pointer a -> Pointer a -> Bool
pointsToSame x y = f x == f y
    where f = pathToTrail.toPath

fromTree :: Tree a -> Pointer a
fromTree t = Pointer Top t

isRoot :: Pointer a -> Bool
isRoot (Pointer Top _ ) = True
isRoot _                = False


filterDown :: (Pointer a -> Bool) -> Pointer a -> [Pointer a]
filterDown pre ptr | isLeaf ptr = [ptr | pre ptr]
filterDown pre ptr              = [ptr | pre ptr] ++ rest
  where
    rest = concat [ maybe [] (filterDown pre) (focusChild c ptr) | c <- ch ptr]
    ch p = fromJust $ children p

-- TODO
-- Can we be cleverer here? So as not to search foo -> bar -> baz
-- foo -> baz -> bar.. That could _actually_ solve the MUS-part.
filterDownBFS :: (Pointer a -> Bool) -> Pointer a -> [Pointer a]
filterDownBFS pre ptr = filter pre (bfs' [ptr])
 where
    bfs' :: [Pointer a] -> [Pointer a]
    bfs' [] = []
    bfs' l  = l ++ bfs'  (concatMap childPointers l)
      where
        childPointers :: Pointer a -> [Pointer a]
        childPointers pr = case children pr of
                     Nothing   -> []
                     Just chen -> catMaybes [ focusChild ch pr | ch <- chen]

focusUp :: Pointer a -> Maybe (Pointer a)
focusUp (Pointer Top _)                                    = Nothing

focusUp (Pointer (PChoicePoint path context q a )       t) = Just $ Pointer path newTree
  where newTree = PChoice q a newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (FChoicePoint path context q a b1 b2 ) t) = Just $ Pointer path newTree
  where newTree = FChoice q a b1 b2 newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (SChoicePoint path context q a b )     t) = Just $ Pointer path newTree
  where newTree = SChoice q a b newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (GChoicePoint path context)            t) = Just $ Pointer path newTree
  where newTree = GoalChoice  newPSQ
        newPSQ  = P.joinContext t context

focusUp (Pointer (FailPoint path c f)                   t) = Just $ Pointer path newTree
  where newTree = Fail c f (Just t)


focusChild :: ChildType -> Pointer a -> Maybe (Pointer a)
focusChild (CTP key)  (Pointer oldPath (PChoice q a       psq)) = Pointer newPath <$> P.lookup key psq
  where newPath = PChoicePoint oldPath newContext q a
        newContext = P.makeContextAt key psq

focusChild (CTF key)  (Pointer oldPath (FChoice q a b1 b2 psq)) = Pointer newPath <$> P.lookup key psq
  where newPath = FChoicePoint oldPath newContext q a b1 b2
        newContext = P.makeContextAt key psq

focusChild (CTS key)  (Pointer oldPath (SChoice q a b     psq)) = Pointer newPath <$> P.lookup key psq
  where newPath = SChoicePoint oldPath newContext q a b
        newContext = P.makeContextAt key psq

focusChild (CTOG key) (Pointer oldPath (GoalChoice        psq)) = Pointer newPath <$> P.lookup key psq
  where newPath = GChoicePoint oldPath newContext
        newContext = P.makeContextAt key psq

focusChild CTFail     (Pointer oldPath (Fail c f     (Just t))) = Just $ Pointer newPath t
  where newPath = FailPoint oldPath c f

focusChild CTFail     (Pointer _        (Fail _ _     Nothing)) = Nothing -- This is sort of bad

focusChild _          _                                         = Nothing




focusRoot :: Pointer a -> Pointer a
focusRoot treePointer = maybe treePointer focusRoot (focusUp treePointer)

isLeaf :: Pointer a -> Bool
isLeaf = isNothing.children

children :: Pointer a -> Maybe [ChildType]
children (Pointer _ (PChoice _ _     c)) = Just $ map CTP  $ P.keys c
children (Pointer _ (FChoice _ _ _ _ c)) = Just $ map CTF  $ P.keys c
children (Pointer _ (SChoice _ _ _   c)) = Just $ map CTS  $ P.keys c
children (Pointer _ (GoalChoice      c)) = Just $ map CTOG $ P.keys c
children (Pointer _ (Fail       _ _  _)) = Just $ [CTFail]
children _                               = Nothing

