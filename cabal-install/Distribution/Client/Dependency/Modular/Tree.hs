{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Client.Dependency.Modular.Tree where

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

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's weak/trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason  (Maybe (FailTree a))
  deriving (Eq, Show, Functor)
  -- Above, a choice is called trivial if it clearly does not matter. The
  -- special case of triviality we actually consider is if there are no new
  -- dependencies introduced by this node.
  --
  -- A (flag) choice is called weak if we do want to defer it. This is the
  -- case for flags that should be implied by what's currently installed on
  -- the system, as opposed to flags that are used to explicitly enable or
  -- disable some functionality.

data FailTree a    = FailTree { failTree  :: (Tree a)
                              , failQPN   :: QPN
                              , failI     :: I }
                                deriving (Eq, Show, Functor)

data FailTreeF a b = FailTreeF b QPN I
    deriving (Functor, Foldable, Traversable, Eq, Show)

innFailTree :: FailTreeF a (Tree a) -> FailTree a
innFailTree (FailTreeF t q i) = FailTree t q i

outFailTree :: FailTree a -> FailTreeF a (Tree a)
outFailTree (FailTree  t q i) = FailTreeF t q i

data NodeType a = NTP QPN a
                | NTF QFN a Bool Bool
                | NTS QSN a Bool
                | NTGoal
                | NTDone RevDepMap
                | NTFail (ConflictSet QPN) FailReason deriving (Eq, Show)


treeToNode :: Tree a -> NodeType a
treeToNode (PChoice     qpn a       _) = NTP qpn a
treeToNode (FChoice     qfn a b1 b2 _) = NTF qfn a b1 b2
treeToNode (SChoice     qsn a b1    _) = NTS qsn a b1
treeToNode (GoalChoice              _) = NTGoal
treeToNode (Done        rdm          ) = NTDone rdm
treeToNode (Fail        cnfset fr   _) = NTFail cnfset fr






data ChildType = CTP I
               | CTF Bool
               | CTS Bool
               | CTOG OpenGoal
               | CTFail (Maybe (QPN, I))
                deriving (Show, Eq)


showChild :: ChildType -> String
showChild (CTP (I ver _))          = showVer ver
showChild (CTF bool)               = show bool
showChild (CTS bool)               = show bool
showChild (CTOG opengoal)          = showOpenGoal opengoal
showChild (CTFail Nothing)         = "Failing Node"
showChild (CTFail (Just (qpn, i))) = "Failing Node: Introduced by the choice " ++ showQPN qpn ++ "-" ++ showI i

isInstalled :: ChildType -> Bool
isInstalled (CTP (I _ (Inst _))) = True
isInstalled _                    = False

data FailReason = InconsistentInitialConstraints
                | Conflicting [Dep QPN]
                | CannotInstall
                | CannotReinstall
                | Shadowed
                | Broken
                | GlobalConstraintVersion VR
                | GlobalConstraintInstalled
                | GlobalConstraintSource
                | GlobalConstraintFlag
                | ManualFlag
                | BuildFailureNotInIndex PN
                | MalformedFlagChoice QFN
                | MalformedStanzaChoice QSN
                | EmptyGoalChoice
                | Backjump
  deriving (Eq, Show)

-- | Functor for the tree type.
data TreeF a b =
    PChoiceF    QPN a           (PSQ I        b)
  | FChoiceF    QFN a Bool Bool (PSQ Bool     b)
  | SChoiceF    QSN a Bool      (PSQ Bool     b)
  | GoalChoiceF                 (PSQ OpenGoal b)
  | DoneF       RevDepMap
  | FailF       (ConflictSet QPN) FailReason (Maybe (FailTreeF a b))
  deriving (Functor, Foldable, Traversable)

out :: Tree a -> TreeF a (Tree a)
out (PChoice    p i     ts) = PChoiceF    p i     ts
out (FChoice    p i b m ts) = FChoiceF    p i b m ts
out (SChoice    p i b   ts) = SChoiceF    p i b   ts
out (GoalChoice         ts) = GoalChoiceF         ts
out (Done       x         ) = DoneF       x
out (Fail       c x     ts) = FailF       c x     (outFailTree <$> ts)

inn :: TreeF a (Tree a) -> Tree a
inn (PChoiceF    p i     ts) = PChoice    p i     ts
inn (FChoiceF    p i b m ts) = FChoice    p i b m ts
inn (SChoiceF    p i b   ts) = SChoice    p i b   ts
inn (GoalChoiceF         ts) = GoalChoice         ts
inn (DoneF       x         ) = Done       x
inn (FailF       c x     ts) = Fail       c x     (innFailTree <$> ts)

-- | Determines whether a tree is active, i.e., isn't a failure node.
active :: Tree a -> Bool
active (Fail _ _ _) = False
active _            = True

-- | Determines how many active choices are available in a node. Note that we
-- count goal choices as having one choice, always.
choices :: Tree a -> Int
choices (PChoice    _ _     ts) = P.length (P.filter active ts)
choices (FChoice    _ _ _ _ ts) = P.length (P.filter active ts)
choices (SChoice    _ _ _   ts) = P.length (P.filter active ts)
choices (GoalChoice         _ ) = 1
choices (Done       _         ) = 1
choices (Fail       _ _     _ ) = 0

-- | Variant of 'choices' that only approximates the number of choices,
-- using 'llength'.
lchoices :: Tree a -> Int
lchoices (PChoice    _ _     ts) = P.llength (P.filter active ts)
lchoices (FChoice    _ _ _ _ ts) = P.llength (P.filter active ts)
lchoices (SChoice    _ _ _   ts) = P.llength (P.filter active ts)
lchoices (GoalChoice         _ ) = 1
lchoices (Done       _         ) = 1
lchoices (Fail       _ _     _ ) = 0

-- | Catamorphism on trees.
cata :: (TreeF a b -> b) -> Tree a -> b
cata phi x = (phi . fmap (cata phi) . out) x

trav :: (TreeF a (Tree b) -> TreeF b (Tree b)) -> Tree a -> Tree b
trav psi x = cata (inn . psi) x

-- | Paramorphism on trees.
para :: (TreeF a (b, Tree a) -> b) -> Tree a -> b
para phi = phi . fmap (\ x -> (para phi x, x)) . out

cataM :: Monad m => (TreeF a b -> m b) -> Tree a -> m b
cataM phi = phi <=< mapM (cataM phi) <=< return . out

-- | Anamorphism on trees.
ana :: (b -> TreeF a b) -> b -> Tree a
ana psi = inn . fmap (ana psi) . psi

anaM :: Monad m => (b -> m (TreeF a b)) -> b -> m (Tree a)
anaM psi = return . inn <=< mapM (anaM psi) <=< psi
