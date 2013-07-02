module Distribution.Client.Dependency.Modular.Solver where

import Data.Map as M

import Distribution.Client.Dependency.Types

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Builder
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Explore
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.Preference as P
import Distribution.Client.Dependency.Modular.Validate
import Distribution.Client.Dependency.Modular.Tree        (Tree)
import Distribution.Client.Dependency.Modular.TreeZipper (Pointer(..))
import Control.Applicative ((<$>))


-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: Bool,
  independentGoals      :: Bool,
  avoidReinstalls       :: Bool,
  shadowPkgs            :: Bool,
  maxBackjumps          :: Maybe Int
}

solve :: SolverConfig ->   -- solver parameters
         Index ->          -- all available packages as an index
         (PN -> PackagePreferences) -> -- preferences
         Map PN [PackageConstraint] -> -- global constraints
         [PN] ->                       -- global goals
         (Maybe (Pointer QGoalReasonChain) -> Log Message (Assignment, RevDepMap))
solve _  _   _         _               _         (Just ptr) = donePtrToLog ptr
solve sc idx userPrefs userConstraints userGoals Nothing    = solveGivenTree tree
  where
    tree = solveTree sc idx userPrefs userConstraints userGoals

solveGivenTree ::Tree QGoalReasonChain -> Log Message (Assignment, RevDepMap)
solveGivenTree = explorePhase        .
                 spaceReductionPhase
  where
    spaceReductionPhase = P.firstGoal
    explorePhase        = exploreTreeLog . backjump


solveTree :: SolverConfig ->   -- solver parameters
         Index ->          -- all available packages as an index
         (PN -> PackagePreferences) -> -- preferences
         Map PN [PackageConstraint] -> -- global constraints
         [PN] ->                       -- global goals
         (Tree QGoalReasonChain)
solveTree sc idx userPrefs userConstraints userGoals =
           heuristicsPhase  $
           preferencesPhase $
           validationPhase  $
           prunePhase       $
           buildPhase
  where

    heuristicsPhase  = -- P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if preferEasyGoalChoices sc
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice
    preferencesPhase = P.preferPackagePreferences userPrefs
    validationPhase  = P.enforceManualFlags . -- can only be done after user constraints
                       P.enforcePackageConstraints userConstraints .
                       validateTree idx
    prunePhase       = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                       -- packages that can never be "upgraded":
                       P.requireInstalled (`elem` [PackageName "base",
                                                   PackageName "ghc-prim"])
    buildPhase       = buildTree idx (independentGoals sc) userGoals


-- This either gives an error, or a pointer to a "Done"-node, ignoring the Log stuff
explorePointer :: Pointer a -> Either String (Pointer a)
explorePointer treePointer = snd <$> (runTreePtrLog  .
                                      explorePhase   .
                                      heuristicsPhase) (toTree treePointer)
  where
    explorePhase     = exploreTreePtrLog treePointer . backjump
    heuristicsPhase  = P.firstGoal -- commit to the first choice (saves space)

-- The tree is given by solveTree, which has done that already.
--                       if preferEasyGoalChoices sc
--                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
--                         else P.preferBaseGoalChoice
