module Distribution.Client.Dependency.Modular.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT,
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), ChildType(..), fromTree, toTree,  children, focusChild, focusUp, isRoot, focusRoot)


import Distribution.Client.Dependency.Modular.Dependency
                (QGoalReasonChain, GoalReason(..), showOpenGoal, showVar, RevDepMap, showDep)


import Distribution.Client.Dependency.Modular.Tree (Tree(..), FailReason(..))

import Distribution.Client.Dependency.Modular.Interactive.Parser

import Data.Maybe (fromJust, fromMaybe)

import Distribution.Client.Dependency.Modular.Package (I(..), Loc(..), showQPN, showPI, unPN)
import Distribution.Client.Dependency.Modular.Flag(showQSN, showQFN)
import Distribution.Client.Dependency.Modular.Version (showVer, showVR)

import Data.Set (toList)
import Prelude hiding (or)
import qualified Data.Map as Map

import Distribution.Client.Dependency.Modular.Log (showLog)
import qualified Distribution.Client.Dependency.Modular.Preference as P

import Distribution.Client.Dependency.Modular.Explore (exploreTreeLog, exploreTreeLogPtr, backjump, runTreeLogPtr)

-- In the long run, I don't think we should have such a (relatively useless) welcome message.
-- Some basic help would be more informative.

runInteractive :: Maybe (Tree QGoalReasonChain) -> IO ()
runInteractive Nothing =
    putStrLn "Ooops.. you chose the wrong solver"
runInteractive (Just searchTree) = do

    putStrLn "Welcome to cabali!"
    putStrLn "go n -- chooses n - alternatively \"n\" does the same. Or just Enter, if there is only one choice"
    putStrLn "up   -- goes up one step"
    putStrLn "top  -- goes all the way to the top"
    putStrLn "log  -- prints the log of an automated run"
    putStrLn "auto -- starts the automatic solver"
    putStrLn ",    -- chains commands (e.g. 1,1,1,1,top does nothing)"

    runInputT defaultSettings (loop $ Just $ fromTree searchTree)
  where
        loop :: Maybe (Pointer QGoalReasonChain) -> InputT IO ()
        loop Nothing = outputStrLn "Bye bye"
        loop (Just treePointer) = do
            outputStrLn $ "Node: " ++ showNodeFromTree ( toTree treePointer )
            outputStrLn "Choices: "
            outputStrLn $ displayChoices treePointer `or` "None"
            tP <- handleCommand treePointer
            loop tP

        or :: String -> String -> String
        "" `or` s = s
        s  `or` _ = s



generateChoices :: Pointer a -> [(Int, ChildType)]
generateChoices treePointer = zip [1..] (fromMaybe [] $ children treePointer)

handleCommand :: Pointer QGoalReasonChain -> InputT IO (Maybe (Pointer QGoalReasonChain))
handleCommand  treePointer = do
  inp <- getInputLine "> "
  case inp of
    Nothing    -> return Nothing
    Just text  -> case readExpr text >>= \cmd -> interpretExpression cmd treePointer of
                    Left s  -> do outputStrLn s
                                  handleCommand treePointer
                    Right t -> return (Just t)


interpretExpression :: Expression -> Pointer QGoalReasonChain ->  Either String (Pointer QGoalReasonChain)
interpretExpression []      _        = error "Internal Error in interpretExpression"
interpretExpression [cmd]   treePos  = interpretCommand treePos cmd
interpretExpression (x:xs)  treePos  = interpretCommand treePos x >>= interpretExpression xs


interpretCommand :: Pointer QGoalReasonChain -> Command -> Either String (Pointer QGoalReasonChain)
interpretCommand treePointer ToTop = Right $ focusRoot treePointer

interpretCommand treePointer Up | isRoot treePointer  = Left "We are at the top"
interpretCommand treePointer Up                       = Right $ fromJust $ focusUp treePointer

interpretCommand treePointer (Go n) = case focused of
                                        Nothing -> Left "No such child"
                                        Just subPointer -> Right subPointer
  where focused = lookup n choices >>= \foo -> focusChild foo treePointer
        choices = generateChoices treePointer

interpretCommand treePointer Empty =  case choices of
                                        [(_, child)] -> Right $ fromJust $ focusChild child treePointer
                                        _            -> Left "Ambiguous choice"
  where choices = generateChoices treePointer

interpretCommand treePointer Auto = runTreeLogPtr foo
    where
    foo              = explorePhase $ heuristicsPhase (pointerTree treePointer)
    explorePhase     = exploreTreeLogPtr treePointer . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice



interpretCommand treePointer AutoLog = Left fooString
  where
    fooString = showLog $ explorePhase $ heuristicsPhase (pointerTree treePointer)
    explorePhase     = exploreTreeLog . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice




displayChoices :: Pointer QGoalReasonChain -> String
displayChoices treePointer = unlines $ map (uncurry makeEntry) $ generateChoices treePointer
  where
    makeEntry :: Int -> ChildType -> String
    makeEntry n child = "(" ++ show n ++ ")  " ++ showChild child ++ " " ++ fromMaybe "" (failReason child)

    failReason :: ChildType -> Maybe String
    failReason child | isFail (focusChild child treePointer)  = Just "\t(fails)"
    failReason _                                              = Nothing

    isFail :: Maybe (Pointer QGoalReasonChain) -> Bool
    isFail (Just (Pointer _ (Fail _ _)))  = True
    isFail _                              = False

--data ChildType = CTP I | CTF Bool | CTS Bool | CTOG OpenGoal deriving (Show)
--data I = I Ver Loc
--data Loc = Inst PId | InRepo
--data OpenGoal = OpenGoal (FlaggedDep QPN) QGoalReasonChain


showChild :: ChildType -> String
showChild (CTP (I ver (Inst _))) = "Version " ++ showVer ver ++ "\t(Installed)"
showChild (CTP (I ver InRepo)) = "Version " ++ showVer ver
showChild (CTF bool) = show bool
showChild (CTS bool) = show bool
showChild (CTOG opengoal) = "OpenGoal: " ++ showOpenGoal opengoal
--showChild (CTOG (OpenGoal flagged qgoalreasonchain)) = "OpenGoal: " ++ (show flagged)
--                                        ++ " Reason: " ++ (show $ head qgoalreasonchain)


showNodeFromTree :: Tree QGoalReasonChain -> String
showNodeFromTree (PChoice qpn a _)           = "PChoice: QPN: " ++ showQPN qpn ++ "\n\t QGoalReason: " ++ showGoalReason a
showNodeFromTree (FChoice qfn a b1 b2 _)     = "FChoice: QFN: " ++ showQFN qfn ++ "\n\t QGoalReason: " ++ showGoalReason a
                                                    ++ "\n\t Bools: " ++ show (b1, b2)
showNodeFromTree (SChoice qsn a b _)         = "SChoice: QSN: " ++ showQSN qsn ++ "\n\t QGoalReason: " ++ showGoalReason a
                                                    ++ "\n\t Bool " ++ show b
showNodeFromTree (GoalChoice _)              = "GoalChoice"
showNodeFromTree (Done rdm)                  = "Done! \nRevDepMap: \n" ++  showRevDepMap rdm
showNodeFromTree (Fail cfs fr)               = "FailReason: " ++ showFailReason fr ++ "\nConflictSet: " ++ showConflictSet cfs
  where showConflictSet s = show $ map showVar (toList s)


showRevDepMap :: RevDepMap -> String
showRevDepMap rdm =  unlines $ map showKey (Map.keys rdm)
  where showKey key = showQPN key ++ ": " ++ showValues key
        showValues key = show (map showQPN (fromJust $ Map.lookup key rdm))

{-
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
-}

showFailReason :: FailReason -> String
showFailReason (Conflicting depQPN)         = "Conflicting: "             ++ show (map showDep depQPN)
showFailReason (MalformedFlagChoice qfn)    = "MalformedFlagChoice: "     ++ showQFN qfn
showFailReason (MalformedStanzaChoice qsn)  = "MalformedStanzaChoice: "   ++ showQSN qsn
showFailReason (BuildFailureNotInIndex pn)  = "BuildFailureNotInIndex: "  ++ unPN pn
showFailReason (GlobalConstraintVersion vr) = "GlobalConstraintVersion: " ++ showVR vr
showFailReason x = show x
{-
type QGoalReason = GoalReason QPN
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show)

data PI qpn = PI qpn I
data I = I Ver Loc

data FN qpn = FN (PI qpn) Flag
-- | Flag identifier. Just a string.
type Flag = FlagName


data SN qpn = SN (PI qpn) OptionalStanza
-}

showGoalReason :: QGoalReasonChain -> String
showGoalReason (PDependency piqpn :_) = "PDependency (depended by): " ++ showPI piqpn
showGoalReason (FDependency fnqpn b : _) = "FDependency: " ++ showQFN fnqpn  ++ " Bool: " ++ show b
showGoalReason (SDependency snqpn :_) = "SDependency: " ++ showQSN snqpn
showGoalReason (UserGoal:_) = "UserGoal"
showGoalReason [] = error "Empty QGoalReasonChain - this should never happen, I think"

-- We should also try to make output between the interactive solver and the -v3 trace similar.
-- So these display functions should go into other modules, so that they can be reused from
-- multiple positions in the code.



{-
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show)
-}