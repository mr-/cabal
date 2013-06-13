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

import Distribution.Client.Dependency.Modular.Explore (exploreTreeLog, exploreTreePtrLog, backjump, runTreePtrLog)

import Control.Applicative ( (<$>) )


data UIState a = UIState {uiPointer :: (Pointer a), uiBreakPoints :: [(String, Pointer a)]}
-- Better uiBreakPoints :: [(String, Pointer a -> Bool)]

-- features: cut
--           autoTill breakpoint (breakPoint in the new sense)

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

    runInputT defaultSettings (loop $ Just $ UIState (fromTree searchTree) [])
  where
        loop :: Maybe (UIState QGoalReasonChain) -> InputT IO ()
        loop Nothing = outputStrLn "Bye bye"
        loop (Just uiState) = do
            outputStrLn $ "Node: " ++ showNodeFromTree ( toTree $ uiPointer uiState )
            outputStrLn "Choices: "
            outputStrLn $ displayChoices (uiPointer uiState) `or` "None"
            uiS <- handleCommand uiState
            loop uiS

        or :: String -> String -> String
        "" `or` s = s
        s  `or` _ = s



generateChoices :: Pointer a -> [(Int, ChildType)]
generateChoices treePointer = zip [1..] (fromMaybe [] $ children treePointer)

handleCommand :: UIState QGoalReasonChain -> InputT IO (Maybe (UIState QGoalReasonChain))
handleCommand uiState = do
  inp <- getInputLine "> "
  case inp of
    Nothing    -> return Nothing
    Just text  -> case readStatements text >>= \cmd -> interpretStatements cmd uiState of
                    Left s  -> do outputStrLn s
                                  handleCommand uiState
                    Right t -> return $ Just t


interpretStatements :: Statements -> UIState QGoalReasonChain ->  Either String (UIState QGoalReasonChain)
interpretStatements (Statements [])      _        = error "Internal Error in interpretExpression"
interpretStatements (Statements [cmd])   uiState  = interpretStatement uiState cmd
interpretStatements (Statements (x:xs))  uiState  = interpretStatement uiState x >>= interpretStatements (Statements xs)


interpretStatement :: UIState QGoalReasonChain -> Statement -> Either String (UIState QGoalReasonChain)
interpretStatement uiState ToTop = Right $ uiState {uiPointer = focusRoot (uiPointer uiState)}

interpretStatement uiState Up | isRoot (uiPointer uiState)  = Left "We are at the top"
interpretStatement uiState Up                               = Right $ uiState { uiPointer = fromJust $ focusUp (uiPointer uiState)}

interpretStatement uiState (Go n) = case focused of
                                        Nothing -> Left "No such child"
                                        Just subPointer -> Right $ uiState {uiPointer = subPointer}
  where focused     = lookup n choices >>= \foo -> focusChild foo treePointer
        choices     = generateChoices treePointer
        treePointer = uiPointer uiState
{-
interpretStatement uiState Empty =  case choices of
                        [(_, child)] -> Right $ uiState {uiPointer = fromJust $ focusChild child treePointer}
                        _            -> Left "Ambiguous choice"
  where choices     = generateChoices treePointer
        treePointer = uiPointer uiState
-}

interpretStatement uiState Auto = (\(_,y) -> uiState {uiPointer = y}) <$> runTreePtrLog treePtrLog
  where
    treePtrLog       = explorePhase $ heuristicsPhase (toTree treePointer)
    explorePhase     = exploreTreePtrLog treePointer . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice
    treePointer = uiPointer uiState

interpretStatement uiState AutoLog = Left fooString
  where
    fooString = showLog $ explorePhase $ heuristicsPhase (toTree treePointer)
    explorePhase     = exploreTreeLog . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice
    treePointer      = uiPointer uiState

interpretStatement uiState (BookSet name) = Right $ addBreakPoint name uiState
  where
    addBreakPoint :: String ->  UIState a -> UIState a
    addBreakPoint s u = u {uiBreakPoints = (s, uiPointer u) : uiBreakPoints u}

interpretStatement uiState BookList = Left $ show $ map fst $ uiBreakPoints uiState

interpretStatement uiState (BookJump name) = case lookup name (uiBreakPoints uiState) of
                                            Nothing -> Left "No such breakpoint."
                                            Just a  -> Right $ uiState {uiPointer = a}


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


showChild :: ChildType -> String
showChild (CTP (I ver (Inst _))) = "Version " ++ showVer ver ++ "\t(Installed)"
showChild (CTP (I ver InRepo)) = "Version " ++ showVer ver
showChild (CTF bool) = show bool
showChild (CTS bool) = show bool
showChild (CTOG opengoal) = "OpenGoal: " ++ showOpenGoal opengoal


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