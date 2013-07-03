module Distribution.Client.Dependency.Modular
         ( modularResolver, modularResolverTree, SolverConfig(..)) where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

import Data.Map as M
         ( fromListWith )
import Distribution.Client.Dependency.Modular.Assignment
         ( Assignment, toCPs )
import Distribution.Client.Dependency.Modular.Dependency
         ( RevDepMap )
import Distribution.Client.Dependency.Modular.ConfiguredConversion
         ( convCP )
import Distribution.Client.Dependency.Modular.IndexConversion
         ( convPIs )
import Distribution.Client.Dependency.Modular.Log
         ( logToProgress )
import Distribution.Client.Dependency.Modular.Package
         ( PN )
import Distribution.Client.Dependency.Modular.Solver
         ( SolverConfig(..), solve, getDepTree, ModularConfig(..) )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, DependencyResolverOptions, PackageConstraint(..) )
import Distribution.Client.InstallPlan
         ( PlanPackage )
import Distribution.System
         ( Platform(..) )
import Distribution.Client.Dependency.Modular.Dependency  ( QGoalReasonChain )
import Distribution.Client.Dependency.Modular.Tree        ( Tree )

-- | Ties the two worlds together: classic cabal-install vs. the modular
-- solver. Performs the necessary translations before and after.
modularResolver :: SolverConfig -> DependencyResolver
modularResolver sc conf@(_,_,iidx,sidx,_,_,_) =
      fmap (uncurry postprocess)     . -- convert install plan
      logToProgress (maxBackjumps sc) . -- convert log format into progress format
      (solve sc $ makeModularConfig sc conf)
  where
    postprocess :: Assignment -> RevDepMap -> [PlanPackage]
    postprocess a rdm = map (convCP iidx sidx) (toCPs a rdm)

modularResolverTree :: SolverConfig -> DependencyResolverOptions -> Tree QGoalReasonChain
modularResolverTree sc conf = getDepTree sc $ makeModularConfig sc conf

makeModularConfig :: SolverConfig -> DependencyResolverOptions -> ModularConfig
makeModularConfig sc (Platform arch os, cid, iidx, sidx, pprefs, pcs, pns)
    = ModularConfig idx pprefs gcs pns
    where
      -- Indices have to be converted into solver-specific uniform index.
      idx    = convPIs os arch cid (shadowPkgs sc) iidx sidx
      -- Constraints have to be converted into a finite map indexed by PN.
      gcs    = M.fromListWith (++) (map (\ pc -> (pcName pc, [pc])) pcs)

      -- Helper function to extract the PN from a constraint.
      pcName :: PackageConstraint -> PN
      pcName (PackageConstraintVersion   pn _) = pn
      pcName (PackageConstraintInstalled pn  ) = pn
      pcName (PackageConstraintSource    pn  ) = pn
      pcName (PackageConstraintFlags     pn _) = pn
      pcName (PackageConstraintStanzas   pn _) = pn


