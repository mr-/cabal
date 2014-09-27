module Distribution.Client.Dependency.Modular.Solver
  (SolverConfig(..), ModularConfig(..), solve, explorePointer, getDepTree)
where

import Data.Map as M

import Distribution.Client.Dependency.Types

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Builder
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Explore (runTreePtrLog, transformLog, exploreTreePtrLog, backjump)
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.Preference as P
import Distribution.Client.Dependency.Modular.Validate
import Distribution.Client.Dependency.Modular.Tree        (Tree)
import Distribution.Client.Dependency.Modular.TreeZipper (Pointer(..), fromTree)


-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: Bool,
  independentGoals      :: Bool,
  avoidReinstalls       :: Bool,
  shadowPkgs            :: Bool,
  strongFlags           :: Bool,
  maxBackjumps          :: Maybe Int
}

data ModularConfig = ModularConfig {
  index             :: Index, -- all available packages as an index
  preferences       :: PN -> PackagePreferences,   -- preferences
  globalConstraints :: Map PN [PackageConstraint], -- global constraints
  globalGoals       :: [PN]                        -- global goals
}


-- | Interface: This is the interface to the actual solver.
-- It has a Pointer as optional parameter and will try to solve below that
-- pointer, if provided.
solve :: SolverConfig ->
         ModularConfig ->
         Maybe (Pointer QGoalReasonChain) -> Log Message (Assignment, RevDepMap)
solve _  _  (Just treePointer)  = transformLog $ findDoneBelow treePointer -- was: donePtrToLog treePointer
solve sc mc  Nothing            = transformLog $ findDoneBelow $ fromTree $ getDepTree sc mc


-- Get the tree, given the configs.
getDepTree :: SolverConfig -> ModularConfig -> Tree QGoalReasonChain
getDepTree sc (ModularConfig idx userPrefs userConstraints userGoals) =
           heuristicsPhase  $
           preferencesPhase $
           validationPhase  $
           prunePhase       $
           buildPhase
  where
    heuristicsPhase  = P.deferWeakFlagChoices .
                       P.preferBaseGoalChoice .
                       if preferEasyGoalChoices sc
                         then P.lpreferEasyGoalChoices
                         else id
    preferencesPhase = P.preferPackagePreferences userPrefs
    validationPhase  = P.enforceManualFlags . -- can only be done after user constraints
                       P.enforcePackageConstraints userConstraints .
                       validateTree idx
    prunePhase       = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                       -- packages that can never be "upgraded":
                       P.requireInstalled (`elem` [ PackageName "base"
                                                  , PackageName "ghc-prim"
                                                  , PackageName "integer-gmp"
                                                  , PackageName "integer-simple"
                                                  ])
    buildPhase       = buildTree idx (independentGoals sc) userGoals


-- This either gives an error (Left: showMessage message from the Log), or a pointer to a "Done"-node
explorePointer :: Pointer a -> Either String (Pointer a)
explorePointer treePointer = runTreePtrLog $ findDoneBelow treePointer


-- | "internal"
-- Try to find a solution below the pointer.
findDoneBelow :: Pointer a -> Log Message (Pointer a)
findDoneBelow treePointer = (explorePhase . spaceReductionPhase . toTree) treePointer
  where
    spaceReductionPhase = P.firstGoal                               -- Commit to the first choice (saves space)
    explorePhase        = exploreTreePtrLog treePointer . backjump

