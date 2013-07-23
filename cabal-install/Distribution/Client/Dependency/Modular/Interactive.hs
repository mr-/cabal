module Distribution.Client.Dependency.Modular.Interactive where

import Control.Applicative                                       ((<$>), (<*>), (<|>))
import Data.Char                                                 (toLower)
import Data.List                                                 (isInfixOf, isPrefixOf)
import Data.Maybe                                                (fromJust, fromMaybe)
import Distribution.Client.Dependency                            (DepResolverParams,
                                                                  resolveDependenciesConfigs)
import Distribution.Client.Dependency.Modular                    (modularResolverTree)
import Distribution.Client.Dependency.Modular.Assignment         (showAssignment)
import Distribution.Client.Dependency.Modular.Dependency         (Dep (..), FlaggedDep (..),
                                                                  OpenGoal (..))
import Distribution.Client.Dependency.Modular.Explore            (ptrToAssignment)
import Distribution.Client.Dependency.Modular.Flag               (unQFN, unQSN)
import Distribution.Client.Dependency.Modular.Interactive.Parser (Selection (..), Selections (..),
                                                                  Statement (..), Statements (..),
                                                                  commandList, readStatements)
import Distribution.Client.Dependency.Modular.Package            (QPN, showQPN)
import Distribution.Client.Dependency.Modular.PSQ                (toLeft)
import Distribution.Client.Dependency.Modular.Solver             (explorePointer)
import Distribution.Client.Dependency.Modular.Tree               (ChildType (..), Tree (..),
                                                                  TreeF (..), isInstalled,
                                                                  showChild, showNodeFromTree, trav)
import Distribution.Client.Dependency.Modular.TreeZipper         (Pointer (..), children,
                                                                  filterBetween, findDown,
                                                                  focusChild, focusRoot, focusUp,
                                                                  fromTree, isRoot, liftToPtr,
                                                                  pointsBelow, toTree)
import Distribution.Client.Dependency.Types                      (Solver (..), QPointer)
import Distribution.Simple.Compiler                              (CompilerId)
import Distribution.System                                       (Platform)
import System.Console.Haskeline                                  (InputT, completeWord,
                                                                  defaultSettings, getInputLine,
                                                                  outputStrLn, runInputT,
                                                                  setComplete)
import System.Console.Haskeline.Completion                       (Completion (..), CompletionFunc)

data UIState = UIState {uiPointer     :: QPointer,
                        uiBookmarks   :: [(String, QPointer)],
                        uiInstall     :: Maybe QPointer,       -- these point
                        uiAutoPointer :: Maybe QPointer,
                        uiHistory     :: [Statement]}       -- to Done

-- Better uiBreakPoints :: [(String, QPointer -> Bool)]
-- features: cut
-- Figure out what the given bools for flags and Stanzas mean.

setAutoPointer :: UIState -> QPointer -> UIState
setAutoPointer state nP = state {uiAutoPointer = Just nP}

setPointer :: UIState -> QPointer -> UIState
setPointer state nP = state {uiPointer = nP}

setInstall :: UIState -> QPointer -> UIState
setInstall state nP = state {uiInstall = Just nP}

runInteractive :: Platform
               -> CompilerId
               -> Solver
               -> DepResolverParams
               -> IO (Maybe QPointer)
runInteractive platform compId solver resolverParams = do
    putStrLn "Welcome to cabali!"
    putStrLn "This interface accepts simple commands separated by ';'. E.g. go 1 ; auto"
    putStrLn "go n            chooses n - alternatively \"n\" does the same. Just Enter, picks the first choice"
    putStrLn "up              goes up one step"
    putStrLn "top             goes all the way to the top"
    putStrLn "auto            starts the automatic solver"
    putStrLn "goto aeson      runs the parser until it sets aeson's version"
    putStrLn "goto aeson:test runs the parser until it sets the flag test for aeson"
    putStrLn "prefer aeson    sorts the choices so that aeson comes first if it is available (Same arguments as goto)"
    putStrLn "bset name       sets a bookmark called name"
    putStrLn "blist           lists all bookmarks"
    putStrLn "bjump name      jumps to the bookmark name"
    putStrLn "indicateAuto    indicates the choices the solver would have made with a little (*)"
    putStrLn "install         Once the interface says 'Done', you can type 'install' to install the package"
    putStrLn "showPlan        shows what is going to be installed/used"
    putStrLn "whatWorks       lists the choices that lead to a valid installplan"
    putStrLn "back            goes back to the last command"

    let (sc, depResOpts) = resolveDependenciesConfigs platform compId solver resolverParams
        searchTree       = modularResolverTree sc depResOpts
        initialState     = Just $ UIState (fromTree searchTree) [] Nothing Nothing []
        completion       = setComplete cmdComplete defaultSettings

    runInputT completion (loop initialState)
      where
        loop :: Maybe UIState -> InputT IO (Maybe QPointer)
        loop Nothing =
          outputStrLn "Bye bye" >> return Nothing
--        which is better?
--        loop (Just uiState) | isInstall uiState =
        loop (Just uiState@UIState{ uiInstall=(Just _) }) =
          outputStrLn "Bye bye" >> return (uiInstall uiState)

        loop (Just uiState) = do
          outputStrLn $ showNodeFromTree ( toTree $ uiPointer uiState )
          outputStrLn $ displayChoices uiState `thisOrThat` "No choices left"
          uiS <- handleCommand uiState
          loop uiS

        thisOrThat :: String -> String -> String
        "" `thisOrThat` s = s
        s  `thisOrThat` _ = s

        cmdComplete :: CompletionFunc IO
        cmdComplete = completeWord Nothing " " completions
          where
            completions :: String -> IO [Completion]
            completions str = return $ map (\f -> Completion f f False) (x str)
            x :: String -> [String]
            x str          = filter (isPrefixOf str) commandList

generateChoices :: QPointer -> [(Int, ChildType)]
generateChoices treePointer = zip [1..] (fromMaybe [] $ children treePointer)

handleCommand :: UIState -> InputT IO (Maybe UIState)
handleCommand uiState = do
  inp <- getInputLine "> "
  case inp of
    Nothing    -> return Nothing
    Just text  -> case readStatements text >>= \cmd -> interpretStatements cmd uiState of
                    Left s  -> do outputStrLn s
                                  handleCommand uiState
                    Right t -> return $ Just t


interpretStatements :: Statements -> UIState ->  Either String UIState
interpretStatements (Statements [])     _        = error "Internal Error in interpretExpression"
interpretStatements (Statements [cmd])  uiState  = addHistory cmd <$> (interpretStatement uiState cmd)
interpretStatements (Statements (c:md)) uiState  = addHistory c   <$> (interpretStatement uiState c) >>= interpretStatements (Statements md)

interpretStatementsWithoutHistory :: Statements -> UIState ->  Either String UIState
interpretStatementsWithoutHistory (Statements [])     _        = error "Internal Error in interpretExpression"
interpretStatementsWithoutHistory (Statements [cmd])  uiState  = interpretStatement uiState cmd
interpretStatementsWithoutHistory (Statements (c:md)) uiState  = interpretStatement uiState c >>= interpretStatementsWithoutHistory (Statements md)



addHistory :: Statement -> UIState -> UIState
addHistory Back uiState = uiState
addHistory statement uiState = uiState {uiHistory = statement:oldHistory}
  where oldHistory = uiHistory uiState

interpretStatement :: UIState -> Statement -> Either String UIState
interpretStatement uiState ToTop = Right $ uiState {uiPointer = focusRoot (uiPointer uiState)}

interpretStatement uiState Up | isRoot (uiPointer uiState)  = Left "We are at the top"
interpretStatement uiState Up                               = Right $ uiState { uiPointer = fromJust $ focusUp (uiPointer uiState)}

interpretStatement uiState (Go n) = case focused of
                                        Nothing -> Left "No such child"
                                        Just subPointer -> Right $ uiState {uiPointer = subPointer}
  where focused     = lookup n choices >>= flip focusChild treePointer
        choices     = generateChoices treePointer
        treePointer = uiPointer uiState

interpretStatement uiState Empty = case choices of -- behave differently when indicateAuto is on?
                        ((_, child):_) -> Right $ setPointer uiState $ fromJust $ focusChild child treePointer
                        _              -> Left "No choice left"
  where choices     = generateChoices treePointer
        treePointer = uiPointer uiState


interpretStatement uiState Auto = autoRun uiState

interpretStatement uiState (BookSet name) = Right $ addBookmark name uiState
  where
    addBookmark :: String ->  UIState -> UIState
    addBookmark s u = u {uiBookmarks = (s, uiPointer u) : uiBookmarks u}

interpretStatement uiState BookList = Left $ show $ map fst $ uiBookmarks uiState

interpretStatement uiState (BookJump name) = case lookup name (uiBookmarks uiState) of
                                            Nothing -> Left "No such bookmark."
                                            Just a  -> Right $ uiState {uiPointer = a}


-- TODO: Should the selected packages get precedence? Or should they be
-- traversed in the given order? (order makes a difference in selection,
-- e.g. when installing cabal-install, goto zlib has fewer options than
-- selecting it manually as early as possible.)
interpretStatement uiState (Goto selections) = (setPointer uiState . selectPointer selections) <$> autoRun uiState
  where
    selectPointer :: Selections -> UIState -> QPointer
    selectPointer sel autoState = last $ deflt <|> found
      where
        found = filterBetween (isSelected sel . toTree) (uiPointer uiState) (uiPointer autoState)
        deflt = [uiPointer autoState]

interpretStatement uiState Install | isDone (uiPointer uiState)
                              = Right $ setInstall uiState $ uiPointer uiState
interpretStatement _ Install  = Left "Need to be on a \"Done\"-Node"
interpretStatement _ (Cut _) = Left "Ooops.. not implemented yet."

interpretStatement uiState (Find sel) = case findDown (isSelected sel.toTree) (uiPointer uiState) of
                                          (x:_) -> Right $ setPointer uiState x
                                          _     -> Left "Nothing found"

interpretStatement uiState IndicateAuto = setAutoPointer uiState <$> (explorePointer . uiPointer) uiState

interpretStatement uiState ShowPlan = Left $ showAssignment $ ptrToAssignment (uiPointer uiState)

interpretStatement uiState (Prefer sel) = Right $ setPointer uiState (preferSelections sel `liftToPtr` uiPointer uiState)

interpretStatement uiState@(UIState {uiHistory = (_:reminder)}) Back =
        interpretStatementsWithoutHistory (Statements (ToTop : reverse reminder)) ( uiState { uiHistory = reminder } )
interpretStatement _ Back = Left "Cannot go back"

interpretStatement (UIState {uiHistory = history}) ShowHistory = Left $ show history

interpretStatement uiState WhatWorks =
                      Left                                    $
                      show                                    $
                      map showChild                           $
                      filter works                            $
                      fromJust                                $
                      children (uiPointer uiState)
  where
    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _         = False
    focus :: ChildType -> QPointer
    focus ch = fromJust $ focusChild ch (uiPointer uiState)
    works :: ChildType -> Bool
    works ch = isRight $ explorePointer $ focus ch


isDone :: QPointer -> Bool
isDone (Pointer _ (Done _ )  ) = True
isDone _                       = False


isSelected :: Selections -> Tree a ->  Bool
isSelected (Selections selections) tree  = or [tree `nodeMatches` selection | selection <- selections]
  where
    nodeMatches :: Tree a -> Selection ->  Bool
    nodeMatches (PChoice qpn _ _)     (SelPChoice pname)        =  pname  `isSubOf` showQPN qpn
    nodeMatches (FChoice qfn _ _ _ _) (SelFSChoice name flag)   = (name   `isSubOf` qfnName)
                                                               && (flag   `isSubOf` qfnFlag)
      where (qfnName, qfnFlag) = unQFN qfn
    nodeMatches (SChoice qsn _ _ _ )  (SelFSChoice name stanza) = (name   `isSubOf` qsnName)
                                                               && (stanza `isSubOf` qsnStanza)
      where (qsnName, qsnStanza) = unQSN qsn
    nodeMatches _                     _                         = False

isSubOf :: String -> String -> Bool
isSubOf x y = lower x `isInfixOf` lower y
  where
    lower :: String -> String
    lower = map toLower -- I think cabal is case-insensitive too

-- data OpenGoal = OpenGoal (FlaggedDep QPN) QGoalReasonChain

--data FlaggedDep qpn =
    --Flagged (FN qpn) FInfo (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
  -- | Stanza  (SN qpn)       (TrueFlaggedDeps qpn)
  -- | Simple (Dep qpn)
  --deriving (Eq, Show)

--data Dep qpn = Dep qpn (CI qpn)

preferSelections :: Selections -> Tree a -> Tree a
preferSelections sel = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (toLeft (depMatchesAny sel) xs)
    go x                = x

    depMatchesAny :: Selections -> OpenGoal -> Bool
    depMatchesAny (Selections selections) (OpenGoal fd _) = any (depMatches fd) selections

    depMatches :: FlaggedDep QPN -> Selection -> Bool
    depMatches (Simple (Dep qpn _)) (SelPChoice name)       = name `isSubOf` showQPN qpn
    depMatches (Flagged qfn _ _ _)  (SelFSChoice name flag) = (name `isSubOf` qfnName) && (flag `isSubOf` qfnFlag)
      where (qfnName, qfnFlag) = unQFN qfn
    depMatches (Stanza qsn _)       (SelFSChoice name flag) = (name `isSubOf` qsnName) && (flag `isSubOf` qsnFlag)
      where (qsnName, qsnFlag) = unQSN qsn
    depMatches _                    _                       = False


autoRun :: UIState -> Either String UIState
autoRun uiState = setPointer uiState <$> (explorePointer . uiPointer) uiState


displayChoices :: UIState -> String
displayChoices uiState = prettyShow $ map (uncurry makeEntry) $ generateChoices treePointer
  where
    treePointer = uiPointer uiState
    makeEntry :: Int -> ChildType -> String
    makeEntry n child = comment n child ++ " " ++ showChild child

    comment :: Int -> ChildType -> String
    comment n child = autoComment child ++ show n ++ installComment child ++ failComment child ++ ":"

    autoComment :: ChildType -> String
    autoComment child | isAuto child = "*"
    autoComment _                    = ""

    installComment :: ChildType -> String
    installComment child | isInstalled child = "I"
    installComment _                         = ""

    failComment :: ChildType -> String
    failComment child | isFail child  = "F"
    failComment _                     = ""

    isAuto :: ChildType -> Bool
    isAuto child = Just True == (pointsBelow <$> uiAutoPointer uiState <*> focusChild child treePointer)

    isFail :: ChildType -> Bool
    isFail child = case focusChild child treePointer of
          (Just (Pointer _ (Fail _ _))) -> True
          _                             -> False

    prettyShow :: [String] -> String
    prettyShow l = unlines $ map concat $ splitEvery nrOfCols paddedList
      where
        paddedList = map (pad maxLen) l
        pad :: Int -> String -> String
        pad n string = string ++ replicate (n - length string ) ' '

        nrOfCols = lineWidth `div` maxLen
        lineWidth = 100
        maxLen = maximum (length <$> l) + 2

        splitEvery :: Int -> [a] -> [[a]]
        splitEvery _ [] = []
        splitEvery n list = first : splitEvery n rest
          where (first,rest) = splitAt n list
