module Distribution.Client.SolveTree where




import Distribution.Client.Dependency.Modular.Solver
import Data.Map as M hiding (map, null)

import Distribution.Client.Dependency.Types

import Distribution.Client.Dependency.Modular.Builder
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.Preference as P
import Distribution.Client.Dependency.Modular.Validate

import qualified Data.Set as S

import Distribution.Client.Targets
import Distribution.Client.Dependency

import Distribution.Client.Setup
         ( GlobalFlags(..)
         , ConfigFlags(..) 
         , ConfigExFlags(..), InstallFlags(..) )

import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..), UseSandbox(..) )
import Distribution.Client.Types as SourcefUpgrade


import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration)
import qualified Distribution.Simple.PackageIndex as PackageIndexPlan
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Setup
         ( HaddockFlags(..),  fromFlag, fromFlagOrDefault )n

import Distribution.Verbosity as Verbosity
         ( Verbosity )

import Distribution.System
         ( Platform(..) )


import Distribution.Client.Dependency.Modular.IndexConversion
         ( convPIs )

import qualified Distribution.Client.PackageIndex as PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
         ( PackageIndex )

import qualified Distribution.Client.Dependency.Modular.Tree as Tree




type InstallContext = ( PackageIndex, SourcePackageDb
                      , [UserTarget], [PackageSpecifier SourcePackage] )

-- TODO: Make InstallArgs a proper datatype with documented fields or just get
-- rid of it completely.
-- | Initial arguments given to 'install' or 'makeInstallContext'.
type InstallArgs = ( PackageDBStack
                   , [Repo]
                   , Compiler
                   , Platform
                   , ProgramConfiguration
                   , UseSandbox
                   , Maybe SandboxPackageInfo
                   , GlobalFlags
                   , ConfigFlags
                   , ConfigExFlags
                   , InstallFlags
                   , HaddockFlags )







makeInstallPlanTree :: Verbosity -> InstallArgs -> InstallContext
                -> IO (Tree.Tree QGoalReasonChain)
makeInstallPlanTree verbosity
  (_, _, comp, platform, _, _, mSandboxPkgInfo,
   _, configFlags, configExFlags, installFlags,
   _)
  (installedPkgIndex, sourcePkgDb,
   _, pkgSpecifiers) = do
    solver <- chooseSolver verbosity (fromFlag (configSolver configExFlags))
              (compilerId comp)
   -- notice verbosity "Resolving dependencies..."
    return $ planPackagesTree comp platform mSandboxPkgInfo solver
      configFlags configExFlags installFlags
      installedPkgIndex sourcePkgDb pkgSpecifiers




planPackagesTree :: Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> Solver
             -> ConfigFlags
             -> ConfigExFlags
             -> InstallFlags
             -> PackageIndexPlan.PackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Tree.Tree QGoalReasonChain
planPackagesTree comp platform mSandboxPkgInfo solver
             configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

        resolveDependenciesTree
          platform (compilerId comp)
          solver
          resolverParams

  --  >>= if onlyDeps then pruneInstallPlan pkgSpecifiers else return

  where
    stanzas = concat
        [ if testsEnabled then [TestStanzas] else []
        , if benchmarksEnabled then [BenchStanzas] else []
        ]
    testsEnabled = fromFlagOrDefault False $ configTests configFlags
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags

    reinstall        = fromFlag (installReinstall        installFlags)
    reorderGoals     = fromFlag (installReorderGoals     installFlags)
    independentGoals' = fromFlag (installIndependentGoals installFlags)
    avoidReinstalls'  = fromFlag (installAvoidReinstalls  installFlags)
    shadowPkgs'       = fromFlag (installShadowPkgs       installFlags)
    maxBackjumps'     = fromFlag (installMaxBackjumps     installFlags)
    upgradeDeps      = fromFlag (installUpgradeDeps      installFlags)
--    onlyDeps         = fromFlag (installOnlyDeps         installFlags)

    resolverParams =

        setMaxBackjumps (if maxBackjumps' < 0 then Nothing
                                             else Just maxBackjumps')

      . setIndependentGoals independentGoals'

      . setReorderGoals reorderGoals

      . setAvoidReinstalls avoidReinstalls'

      . setShadowPkgs shadowPkgs'

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- configPreferences configExFlags ]

      . addConstraints
          -- version constraints from the config file or command line
            (map userToPackageConstraint (configExConstraints configExFlags))

      . addConstraints
          --FIXME: this just applies all flags to all targets which
          -- is silly. We should check if the flags are appropriate
          [ PackageConstraintFlags (pkgSpecifierTarget pkgSpecifier) flags
          | let flags = configConfigurationsFlags configFlags
          , not (null flags)
          , pkgSpecifier <- pkgSpecifiers ]

      . addConstraints
          [ PackageConstraintStanzas (pkgSpecifierTarget pkgSpecifier) stanzas
          | pkgSpecifier <- pkgSpecifiers ]

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      . (if reinstall then reinstallTargets else id)

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb pkgSpecifiers




resolveDependenciesTree :: Platform
                    -> CompilerId
                    -> Solver
                    -> DepResolverParams
                    -> Tree.Tree QGoalReasonChain

    --TODO: is this needed here? see dontUpgradeBasePackage
resolveDependenciesTree platform comp  solver params =

    
  modularResolverTree (SolverConfig reorderGoals indGoals noReinstalls
                      shadowing maxBkjumps)
                     platform comp installedPkgIndex sourcePkgIndex
                     preferences constraints targets
  where
    DepResolverParams
      targets constraints
      prefs defpref
      installedPkgIndex
      sourcePkgIndex
      reorderGoals
      indGoals
      noReinstalls
      shadowing
      maxBkjumps      = dontUpgradeBasePackage
                      -- TODO:
                      -- The modular solver can properly deal with broken
                      -- packages and won't select them. So the
                      -- 'hideBrokenInstalledPackages' function should be moved
                      -- into a module that is specific to the Topdown solver.
                      . (if solver /= Modular then hideBrokenInstalledPackages
                                              else id)
                      $ params

    preferences = interpretPackagesPreference
                    (S.fromList targets) defpref prefs


modularResolverTree :: SolverConfig 
                       -> Platform
                       -> CompilerId
                       -> InstalledPackageIndex.PackageIndex
                       -> PackageIndex.PackageIndex SourcePackage
                       -> (PackageName -> PackagePreferences)
                       -> [PackageConstraint]
                       -> [PackageName]
                       -> Tree.Tree QGoalReasonChain
modularResolverTree sc (Platform arch os) cid iidx sidx pprefs pcs pns =
  solveTree sc idx pprefs gcs pns
    where
      -- Indices have to be converted into solver-specific uniform index.
      idx    = convPIs os arch cid (shadowPkgs sc) iidx sidx
      -- Constraints have to be converted into a finite map indexed by PN.
      gcs    = M.fromListWith (++) (map (\ pc -> (pcName pc, [pc])) pcs)

      -- Results have to be converted into an install plan.
--      postprocess :: Assignment -> RevDepMap -> [PlanPackage]
--      postprocess a rdm = map (convCP iidx sidx) (toCPs a rdm)

      -- Helper function to extract the PN from a constraint.
      pcName :: PackageConstraint -> PN
      pcName (PackageConstraintVersion   pn _) = pn
      pcName (PackageConstraintInstalled pn  ) = pn
      pcName (PackageConstraintSource    pn  ) = pn
      pcName (PackageConstraintFlags     pn _) = pn
      pcName (PackageConstraintStanzas   pn _) = pn



solveTree :: SolverConfig ->   -- solver parameters
         Index ->          -- all available packages as an index
         (PN -> PackagePreferences) -> -- preferences
         Map PN [PackageConstraint] -> -- global constraints
         [PN] ->                       -- global goals
         Tree.Tree QGoalReasonChain
solveTree sc idx userPrefs userConstraints userGoals =
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    --explorePhase     = exploreTreeLog . backjump
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