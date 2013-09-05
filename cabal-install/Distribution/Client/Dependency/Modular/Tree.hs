module Distribution.Client.Dependency.Modular.Tree where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM)


import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P hiding (map, toList)
import Distribution.Client.Dependency.Modular.Version

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason (Maybe (Tree a))
  deriving (Eq, Show)
  -- Above, a choice is called trivial if it clearly does not matter. The
  -- special case of triviality we actually consider is if there are no new
  -- dependencies introduced by this node.

data NodeType a = NTP QPN a
                | NTF QFN a Bool Bool
                | NTS QSN a Bool
                | NTGoal
                | NTDone RevDepMap
                | NTFail (ConflictSet QPN) FailReason deriving (Eq, Show)
-- TODO: NTFail wants the tree?

treeToNode :: Tree a -> NodeType a
treeToNode (PChoice     qpn a       _) = NTP qpn a
treeToNode (FChoice     qfn a b1 b2 _) = NTF qfn a b1 b2
treeToNode (SChoice     qsn a b1    _) = NTS qsn a b1
treeToNode (GoalChoice              _) = NTGoal
treeToNode (Done        rdm          ) = NTDone rdm
treeToNode (Fail        cnfset fr   _) = NTFail cnfset fr


-- This does not really belong here, it is too specific to the interactive
-- solver
showNodeFromTree :: Tree QGoalReasonChain -> String
showNodeFromTree (PChoice qpn (UserGoal:_) _) = "Version of " ++ showQPN qpn
showNodeFromTree (PChoice qpn a _)            = showQPN qpn ++ " (needed by " ++ showGoalReason a ++ ")"
showNodeFromTree (FChoice qfn _ b1 b2 _)      = "Flag: " ++ showQFN qfn ++ "\t " ++ trivial ++ " " ++ manual
    where manual  = if b2 then "manual" else "automatic"
          trivial = if b1 then "trivial (no deps introduced by this)" else "not trivial (will introduce deps)"
showNodeFromTree (SChoice qsn _ b _)          = "Stanza: " ++ showQSN qsn -- The "reason" is obvious here
                                                    ++ "\n\t " ++ trivial
  where trivial = if b then "trivial (no deps introduced by this)" else "not trivial (will introduce deps)"
showNodeFromTree (GoalChoice _)               = "Missing dependencies"
showNodeFromTree (Done _rdm)                  = "Done"
showNodeFromTree (Fail cfs fr _)              = "FailReason: " ++ showFailReason fr ++ "\nConflictSet: " ++ showConflictSet cfs
  where showConflictSet s = show $ map showVar (toList s)


instance Functor Tree where
  fmap  f (PChoice qpn i     xs) = PChoice qpn (f i)     (fmap (fmap f) xs)
  fmap  f (FChoice qfn i b m xs) = FChoice qfn (f i) b m (fmap (fmap f) xs)
  fmap  f (SChoice qsn i b   xs) = SChoice qsn (f i) b   (fmap (fmap f) xs)
  fmap  f (GoalChoice        xs) = GoalChoice            (fmap (fmap f) xs)
  fmap _f (Done    rdm         ) = Done    rdm
  fmap  f (Fail    cs fr     t ) = Fail    cs fr         (fmap (fmap f) t)



data ChildType = CTP I
               | CTF Bool
               | CTS Bool
               | CTOG OpenGoal
               | CTFail deriving (Show, Eq)


showChild :: ChildType -> String
showChild (CTP (I ver _))        = showVer ver
showChild (CTF bool)             = show bool
showChild (CTS bool)             = show bool
showChild (CTOG opengoal)        = showOpenGoal opengoal
showChild (CTFail)               = "Failing Node"

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


showFailReason :: FailReason -> String
showFailReason (Conflicting depQPN)         = "Conflicting: "             ++ show (map showDep depQPN)
showFailReason (MalformedFlagChoice qfn)    = "MalformedFlagChoice: "     ++ showQFN qfn
showFailReason (MalformedStanzaChoice qsn)  = "MalformedStanzaChoice: "   ++ showQSN qsn
showFailReason (BuildFailureNotInIndex pn)  = "BuildFailureNotInIndex: "  ++ unPN pn
showFailReason (GlobalConstraintVersion vr) = "GlobalConstraintVersion: " ++ showVR vr
showFailReason x = show x


-- | Functor for the tree type.
data TreeF a b =
    PChoiceF    QPN a           (PSQ I        b)
  | FChoiceF    QFN a Bool Bool (PSQ Bool     b)
  | SChoiceF    QSN a Bool      (PSQ Bool     b)
  | GoalChoiceF                 (PSQ OpenGoal b)
  | DoneF       RevDepMap
  | FailF       (ConflictSet QPN) FailReason (Maybe b)

out :: Tree a -> TreeF a (Tree a)
out (PChoice    p i     ts) = PChoiceF    p i     ts
out (FChoice    p i b m ts) = FChoiceF    p i b m ts
out (SChoice    p i b   ts) = SChoiceF    p i b   ts
out (GoalChoice         ts) = GoalChoiceF         ts
out (Done       x         ) = DoneF       x
out (Fail       c x     ts) = FailF       c x     ts

inn :: TreeF a (Tree a) -> Tree a
inn (PChoiceF    p i     ts) = PChoice    p i     ts
inn (FChoiceF    p i b m ts) = FChoice    p i b m ts
inn (SChoiceF    p i b   ts) = SChoice    p i b   ts
inn (GoalChoiceF         ts) = GoalChoice         ts
inn (DoneF       x         ) = Done       x
inn (FailF       c x     ts) = Fail       c x     ts

instance Functor (TreeF a) where
  fmap f (PChoiceF    p i     ts) = PChoiceF    p i     (fmap f ts)
  fmap f (FChoiceF    p i b m ts) = FChoiceF    p i b m (fmap f ts)
  fmap f (SChoiceF    p i b   ts) = SChoiceF    p i b   (fmap f ts)
  fmap f (GoalChoiceF         ts) = GoalChoiceF         (fmap f ts)
  fmap _ (DoneF       x         ) = DoneF       x
  fmap f (FailF       c x     ts) = FailF       c x     (fmap f ts)

instance Foldable (TreeF a) where
  foldr op e (PChoiceF    _ _     ts) = foldr op e ts
  foldr op e (FChoiceF    _ _ _ _ ts) = foldr op e ts
  foldr op e (SChoiceF    _ _ _   ts) = foldr op e ts
  foldr op e (GoalChoiceF         ts) = foldr op e ts
  foldr _  e (DoneF       _         ) = e
  foldr op e (FailF       _ _     ts) = foldr op e ts

instance Traversable (TreeF a) where
  traverse f (PChoiceF    p i     ts) = PChoiceF    <$> pure p <*> pure i <*>                       traverse f ts
  traverse f (FChoiceF    p i b m ts) = FChoiceF    <$> pure p <*> pure i <*> pure b <*> pure m <*> traverse f ts
  traverse f (SChoiceF    p i b   ts) = SChoiceF    <$> pure p <*> pure i <*> pure b <*>            traverse f ts
  traverse f (GoalChoiceF         ts) = GoalChoiceF <$>                                             traverse f ts
  traverse _ (DoneF       x         ) = DoneF       <$> pure x
  traverse f (FailF       c x     ts) = FailF       <$> pure c <*> pure x <*>                       traverse f ts

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
