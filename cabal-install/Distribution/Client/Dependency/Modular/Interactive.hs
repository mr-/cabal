module Distribution.Client.Dependency.Modular.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT,
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), ChildType(..), fromTree, toTree,  children, focusChild, focusUp, isRoot, focusRoot, filterBetween, findDown)


import Distribution.Client.Dependency.Modular.Dependency
                (QGoalReasonChain, GoalReason(..), showOpenGoal, showVar, RevDepMap, showDep)


import Distribution.Client.Dependency.Modular.Tree (Tree(..), FailReason(..))

import Distribution.Client.Dependency.Modular.Interactive.Parser

import Data.Maybe (fromJust, fromMaybe, isJust)

import Distribution.Client.Dependency.Modular.Package (I(..), Loc(..), showQPN, showPI, unPN)
import Distribution.Client.Dependency.Modular.Flag (showQSN, showQFN, unQFN, unQSN)
import Distribution.Client.Dependency.Modular.Version (showVer, showVR)

import Data.Set (toList)

import qualified Data.Map as Map

import Distribution.Client.Dependency.Modular.Log (showLog)
import qualified Distribution.Client.Dependency.Modular.Preference as P

import Distribution.Client.Dependency.Modular.Explore (exploreTreeLog, exploreTreePtrLog, backjump, runTreePtrLog)

import Control.Applicative ( (<$>), (<|>) )

import Data.List (isInfixOf)

data UIState a = UIState {uiPointer :: Pointer a,
                          uiBookmarks :: [(String, Pointer a)],
                          uiInstall :: Maybe (Pointer a) }

setPointer :: UIState a -> Pointer a -> UIState a
setPointer state nP = state {uiPointer = nP}

setInstall :: UIState a -> Pointer a -> UIState a
setInstall state nP = state {uiInstall = Just nP}

isInstall :: UIState a -> Bool
isInstall = isJust.uiInstall

noInstall :: UIState a -> Bool
noInstall = not.isJust.uiInstall

getInstall :: UIState a -> Pointer a
getInstall (UIState _ _ (Just x)) = x
getInstall _                      = error "Ouch.. got wrong install"
-- Better uiBreakPoints :: [(String, Pointer a -> Bool)]

-- features: cut

runInteractive :: Maybe (Tree QGoalReasonChain) -> IO (Maybe (Pointer QGoalReasonChain))
runInteractive Nothing =
    putStrLn "Ooops.. you chose the wrong solver" >> return Nothing
runInteractive (Just searchTree) = do

    putStrLn "Welcome to cabali!"
    putStrLn "go n                 chooses n - alternatively \"n\" does the same. Or just Enter, if there is only one choice"
    putStrLn "up                   goes up one step"
    putStrLn "top                  goes all the way to the top"
    putStrLn "autoLog              prints the log of an automated run"
    putStrLn "auto                 starts the automatic solver"
    putStrLn "goto aeson           runs the parser until it sets aeson's version"
    putStrLn "goto aeson:developer runs the parser until it sets the flag developer for aeson"
    putStrLn ";                    chains commands (e.g. 1;1;1;top does nothing)"

    runInputT defaultSettings (loop $ Just $ UIState (fromTree searchTree) [] Nothing)
  where
        loop :: Maybe (UIState QGoalReasonChain) -> InputT IO (Maybe (Pointer QGoalReasonChain))
        loop Nothing = outputStrLn "Bye bye" >> return Nothing
        loop (Just uiState) | noInstall uiState = do
            outputStrLn $ "Node: " ++ showNodeFromTree ( toTree $ uiPointer uiState )
            outputStrLn $ displayChoices (uiPointer uiState) `thisOrThat` "None"
            uiS <- handleCommand uiState
            loop uiS
        loop (Just uiState) | isInstall uiState = return $ uiInstall uiState
        thisOrThat :: String -> String -> String
        "" `thisOrThat` s = s
        s  `thisOrThat` _ = s



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
  where focused     = lookup n choices >>= flip focusChild treePointer
        choices     = generateChoices treePointer
        treePointer = uiPointer uiState
{-
interpretStatement uiState Empty =  case choices of
                        [(_, child)] -> Right $ uiState {uiPointer = fromJust $ focusChild child treePointer}
                        _            -> Left "Ambiguous choice"
  where choices     = generateChoices treePointer
        treePointer = uiPointer uiState
-}

interpretStatement uiState Auto = autoRun uiState

interpretStatement uiState AutoLog = Left fooString
  where
    fooString = showLog $ explorePhase $ heuristicsPhase (toTree treePointer)
    explorePhase     = exploreTreeLog . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice
    treePointer      = uiPointer uiState

interpretStatement uiState (BookSet name) = Right $ addBookmark name uiState
  where
    addBookmark :: String ->  UIState a -> UIState a
    addBookmark s u = u {uiBookmarks = (s, uiPointer u) : uiBookmarks u}

interpretStatement uiState BookList = Left $ show $ map fst $ uiBookmarks uiState

interpretStatement uiState (BookJump name) = case lookup name (uiBookmarks uiState) of
                                            Nothing -> Left "No such bookmark."
                                            Just a  -> Right $ uiState {uiPointer = a}

interpretStatement uiState (Goto selections) = (setPointer uiState . selectPointer selections) <$> autoRun uiState
  where
    selectPointer :: Selections -> UIState QGoalReasonChain -> Pointer QGoalReasonChain
    selectPointer sel autoState = last $ deflt <|> found
      where
        found = filterBetween (isSelected sel) (uiPointer uiState) (uiPointer autoState)
        deflt = [uiPointer autoState]

interpretStatement uiState Install | isDone (uiPointer uiState)
                              = Right $ setInstall uiState $ uiPointer uiState
    where
      isDone (Pointer _ (Done _ )  ) = True
      isDone _                       = False

interpretStatement _ Install  = Left "Need to be on a \"Done\"-Node"
interpretStatement _ (Cut _) = Left "Ooops.. not implemented yet."

interpretStatement uiState (Find sel) = case findDown (isSelected sel) (uiPointer uiState) of
                                          (x:_) -> Right $ setPointer uiState x
                                          _     -> Left "Nothing found"


isSelected :: Selections -> Pointer QGoalReasonChain ->  Bool
isSelected (Selections selections) pointer  = or [pointer `matches` selection | selection <- selections]
  where
    matches :: Pointer a -> Selection ->  Bool
    matches (Pointer _ (PChoice qpn _ _))     (SelPChoice pname)         =  pname `isInfixOf` showQPN qpn

    matches (Pointer _ (FChoice qfn _ _ _ _)) (SelFSChoice name flag)    = (name `isInfixOf` qfnName)
                                                                        && (flag `isInfixOf` qfnFlag)
      where
        (qfnName, qfnFlag) = unQFN qfn
    matches (Pointer _ (SChoice qsn _ _ _ ))  (SelFSChoice name stanza) = (name `isInfixOf` qsnName)
                                                                       && (stanza `isInfixOf` qsnStanza)
      where
        (qsnName, qsnStanza) = unQSN qsn
    matches _          _                                 = False


autoRun :: UIState QGoalReasonChain-> Either String (UIState QGoalReasonChain)
autoRun uiState = (\(_,y) -> uiState {uiPointer = y}) <$> runTreePtrLog treePtrLog
  where
    treePtrLog       = explorePhase $ heuristicsPhase (toTree treePointer)
    explorePhase     = exploreTreePtrLog treePointer . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       if False
                         then P.preferBaseGoalChoice . P.deferDefaultFlagChoices . P.lpreferEasyGoalChoices
                         else P.preferBaseGoalChoice
    treePointer = uiPointer uiState


displayChoices :: Pointer QGoalReasonChain -> String
displayChoices treePointer = prettyShow $ map (uncurry makeEntry) $ generateChoices treePointer
  where
    makeEntry :: Int -> ChildType -> String
    makeEntry n child = "(" ++ show n ++ ") " ++ showChild child ++ " " ++ fromMaybe "" (failReason child)

    failReason :: ChildType -> Maybe String
    failReason child | isFail (focusChild child treePointer)  = Just "(F)"
    failReason _                                              = Nothing

    isFail :: Maybe (Pointer QGoalReasonChain) -> Bool
    isFail (Just (Pointer _ (Fail _ _)))  = True
    isFail _                              = False

    prettyShow :: [String] -> String
    prettyShow l = unlines $ map concat $ splitEvery nrOfCols paddedList
      where
        paddedList = map (pad (maxLen+1)) l
        pad :: Int -> String -> String
        pad n string = string ++ replicate (n - length string ) ' '

        nrOfCols = lineWidth `div` maxLen
        lineWidth = 100
        maxLen = maximum (length <$> l) + 1

        splitEvery :: Int -> [a] -> [[a]]
        splitEvery _ [] = []
        splitEvery n list = first : splitEvery n rest
          where (first,rest) = splitAt n list


showChild :: ChildType -> String
showChild (CTP (I ver (Inst _))) = showVer ver ++ " (I)"
showChild (CTP (I ver InRepo))   = showVer ver
showChild (CTF bool)             = show bool
showChild (CTS bool)             = show bool
showChild (CTOG opengoal)        = showOpenGoal opengoal


showNodeFromTree :: Tree QGoalReasonChain -> String
showNodeFromTree (PChoice qpn (UserGoal:_) _) = "Choose a version for " ++ showQPN qpn
showNodeFromTree (PChoice qpn a _)            = showQPN qpn ++ " (needed by " ++ showGoalReason a ++ ")"
showNodeFromTree (FChoice qfn _ b1 b2 _)      = "Flag: " ++ showQFN qfn ++ "\t Bools: " ++ show (b1, b2) -- what do the bools mean?
showNodeFromTree (SChoice qsn _ b _)          = "Stanza: " ++ showQSN qsn -- The "reason" is obvious here
                                                    ++ "\n\t Bool: " ++ show b -- But what do the bools mean?
showNodeFromTree (GoalChoice _)               = "Missing dependencies"
showNodeFromTree (Done rdm)                   = "Done! \nRevDepMap: \n" ++  showRevDepMap rdm
showNodeFromTree (Fail cfs fr)                = "FailReason: " ++ showFailReason fr ++ "\nConflictSet: " ++ showConflictSet cfs
  where showConflictSet s = show $ map showVar (toList s)

{-
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
-}

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
showGoalReason (PDependency piqpn :_) = showPI piqpn
showGoalReason (FDependency fnqpn b : _) = showQFN fnqpn  ++ " Bool: " ++ show b
showGoalReason (SDependency snqpn :_) =  showQSN snqpn
showGoalReason (UserGoal:_) = "UserGoal"
showGoalReason [] = error "Empty QGoalReasonChain - this should never happen, I think"

{-
showGoalReason :: QGoalReasonChain -> String
showGoalReason (PDependency piqpn :_) = "PDependency (depended by): " ++ showPI piqpn
showGoalReason (FDependency fnqpn b : _) = "FDependency: " ++ showQFN fnqpn  ++ " Bool: " ++ show b
showGoalReason (SDependency snqpn :_) = "SDependency: " ++ showQSN snqpn
showGoalReason (UserGoal:_) = "UserGoal"
showGoalReason [] = error "Empty QGoalReasonChain - this should never happen, I think"
-}
-- We should also try to make output between the interactive solver and the -v3 trace similar.
-- So these display functions should go into other modules, so that they can be reused from
-- multiple positions in the code.