module Distribution.Client.Dependency.Modular.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT,
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), fromTree, toTree,  children, focusChild, focusUp, isRoot, focusRoot, filterBetween, findDown)


import Distribution.Client.Dependency.Modular.Dependency
                (QGoalReasonChain)


import Distribution.Client.Dependency.Modular.Tree (Tree(..), ChildType(..), showChild, showNodeFromTree)

import Distribution.Client.Dependency.Modular.Interactive.Parser

import Data.Maybe (fromJust, fromMaybe, isJust)

import Distribution.Client.Dependency.Modular.Package (showQPN)
import Distribution.Client.Dependency.Modular.Flag (unQFN, unQSN)

import Distribution.Client.Dependency.Modular.Log (showLog)
import qualified Distribution.Client.Dependency.Modular.Preference as P

import Distribution.Client.Dependency.Modular.Explore (exploreTreeLog, exploreTreePtrLog, backjump, runTreePtrLog)

import Control.Applicative ( (<$>), (<|>) )

import Data.List (isInfixOf)

import Data.Char (toLower)

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
noInstall = not.isInstall

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
        loop Nothing =
          outputStrLn "Bye bye" >> return Nothing

        loop (Just uiState) | isInstall uiState =
          outputStrLn "Bye bye" >> return (uiInstall uiState)

        loop (Just uiState) = do
          outputStrLn $ "Node: " ++ showNodeFromTree ( toTree $ uiPointer uiState )
          outputStrLn $ displayChoices (uiPointer uiState) `thisOrThat` "No choices left"
          uiS <- handleCommand uiState
          loop uiS

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

interpretStatement uiState Empty =  case choices of
                        ((_, child):_) -> Right $ setPointer uiState $ fromJust $ focusChild child treePointer
                        _              -> Left "No choice left"
  where choices     = generateChoices treePointer
        treePointer = uiPointer uiState


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
    matches (Pointer _ (PChoice qpn _ _))     (SelPChoice pname)         =   pname   `isSubOf` showQPN qpn

    matches (Pointer _ (FChoice qfn _ _ _ _)) (SelFSChoice name flag)    =   (name   `isSubOf` qfnName)
                                                                          && (flag   `isSubOf` qfnFlag)
      where
        (qfnName, qfnFlag) = unQFN qfn
    matches (Pointer _ (SChoice qsn _ _ _ ))  (SelFSChoice name stanza)  =   (name   `isSubOf` qsnName)
                                                                          && (stanza `isSubOf` qsnStanza)
      where
        (qsnName, qsnStanza) = unQSN qsn
    matches _          _                                 = False

    isSubOf :: String -> String -> Bool
    isSubOf x y = lower x `isInfixOf` lower y
      where
        lower :: String -> String
        lower s = map toLower s

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





{-
showRevDepMap :: RevDepMap -> String
showRevDepMap rdm =  unlines $ map showKey (Map.keys rdm)
  where showKey key = showQPN key ++ ": " ++ showValues key
        showValues key = show (map showQPN (fromJust $ Map.lookup key rdm))
-}