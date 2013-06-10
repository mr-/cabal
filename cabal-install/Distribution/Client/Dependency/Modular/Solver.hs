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
import Data.Maybe (fromJust)


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
         (Log Message (Assignment, RevDepMap), Maybe (Tree QGoalReasonChain))
solve sc idx userPrefs userConstraints userGoals = (slog, Just tree)
  where

    slog = explorePhase     $
          heuristicsPhase   $ 
          tree

    tree = preferencesPhase $
           validationPhase  $
           prunePhase       $
           buildPhase
  
    explorePhase     = exploreTreeLog . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
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
