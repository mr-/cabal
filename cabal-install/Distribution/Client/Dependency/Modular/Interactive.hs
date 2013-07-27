module Distribution.Client.Dependency.Modular.Interactive where

import Control.Applicative                                       ((<$>), (<*>), (<|>))
import Control.Monad.State                                       (StateT(..), gets, get, lift, evalStateT, modify)
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
                                                                  fromTree, liftToPtr,
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
                        uiInstall     :: Maybe QPointer,
                        uiAutoPointer :: Maybe QPointer,
                        uiHistory     :: [Statement]}

type AppState = StateT UIState (InputT IO)
data Action = InstallNow | Abort | Continue


-- Better uiBreakPoints :: [(String, QPointer -> Bool)]
-- features: cut
-- Figure out what the given bools for flags and Stanzas mean.


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
        initialState     = UIState (fromTree searchTree) [] Nothing Nothing []
        completion       = setComplete cmdComplete defaultSettings

    runInputT completion $ evalStateT (loop Continue) initialState
      where
        loop :: Action -> AppState (Maybe QPointer)
        loop Abort = do
          lift $ outputStrLn "Bye bye"
          return Nothing

        loop InstallNow = do
          toInstall <- gets uiInstall
          lift $ outputStrLn "Bye bye"
          return toInstall

        loop Continue = do
          uiState <- get
          lift $ outputStrLn $ showNodeFromTree ( toTree $ uiPointer uiState )
          lift $ outputStrLn $ displayChoices uiState `thisOrThat` "No choices left"
          action <- handleCommand
          loop action

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

handleCommand :: AppState Action
handleCommand = do
  inp <- lift $ getInputLine "> "
  case inp of
    Nothing    -> return Abort
    Just text  -> case readStatements text of
                    Left s     -> do lift $ outputStrLn s
                                     handleCommand
                    Right cmds -> do results <- interpretStatements cmds
                                     lift $ outputStrLn $ showResults results
                                     install <- gets uiInstall
                                     return (if isInstall install then InstallNow else Continue)
  where
   isInstall Nothing = False
   isInstall _       = True

showResults :: [Result] -> String
showResults = show

data Result = Error String | Progress String | Success deriving (Show, Eq)

interpretStatements :: Statements -> AppState [Result]
interpretStatements (Statements [])     = error "Internal Error in interpretExpression"
interpretStatements (Statements [cmd])  = do
      addHistory cmd
      result <- interpretStatement cmd
      return [result]

interpretStatements (Statements (c:md)) = do
      addHistory c
      result <- interpretStatement c
      case result of
        (Error _ ) -> return [result] -- return the result and stop processing commands.
        _          -> do list   <- interpretStatements (Statements md)
                         return (result:list)

addHistory :: Statement -> AppState ()
addHistory Back      = return ()
addHistory statement = modify (\uiState -> uiState {uiHistory = statement:uiHistory uiState})

setPointer' :: QPointer -> AppState ()
setPointer' ptr = modifyPointer (const ptr)

modifyPointer :: (QPointer -> QPointer) -> AppState ()
modifyPointer f = do ptr <- gets uiPointer
                     modify (\uiState ->uiState{uiPointer = f ptr})


interpretStatement :: Statement -> AppState Result
interpretStatement ToTop = modifyPointer focusRoot >> return Success

interpretStatement Up = do
  ptr <- gets uiPointer
  setPointer' (fromMaybe ptr (focusUp ptr))
  return Success

interpretStatement (Go n) = do
  treePointer <- gets uiPointer
  let choices = generateChoices treePointer
      focused = lookup n choices >>= flip focusChild treePointer
  case focused of
    Nothing -> return $ Error "No such child"
    Just subPointer -> do setPointer' subPointer
                          return Success

interpretStatement Empty = interpretStatement (Go 1)

interpretStatement Auto = do
  pointer <- gets uiPointer
  case explorePointer pointer of
    Left e  -> return $ Error e
    Right t -> setPointer' t >> return Success -- TODO: Progress?


interpretStatement (BookSet name) = addBookmark name >> return Success
  where
    addBookmark :: String -> AppState ()
    addBookmark s = modify (\u -> u {uiBookmarks = (s, uiPointer u) : uiBookmarks u})

interpretStatement BookList = do bookmarks <- gets uiBookmarks
                                 return $ Progress $ show $ map fst bookmarks

interpretStatement (BookJump name) = do
    bookmarks <- gets uiBookmarks
    case lookup name bookmarks of
        Nothing -> return $ Error "No such bookmark."
        Just a  -> setPointer' a >> return Success


-- TODO: Should the selected packages get precedence? Or should they be
-- traversed in the given order? (order makes a difference in selection,
-- e.g. when installing cabal-install, goto zlib has fewer options than
-- selecting it manually as early as possible.)
interpretStatement (Goto selections) = do
    pointer <- gets uiPointer
    case explorePointer pointer of
        Left e  -> return $ Error e
        Right t -> setPointer' (selectPointer selections pointer t) >> return Success -- TODO: Progress
    where
      selectPointer :: Selections -> QPointer -> QPointer -> QPointer
      selectPointer sel here done = head $ found <|> [done]
        where
          found = filterBetween (isSelected sel . toTree) here done

interpretStatement Install = do ptr <- gets uiPointer
                                case isDone ptr of
                                  True -> modify (\x -> x{uiInstall = Just ptr}) >> return Success
                                  False -> return $ Error "Need to be on a \"Done\"-Node"

interpretStatement (Cut _) = return $ Error "Ooops.. not implemented yet."

 -- Need to make that more efficient, prune tree first. (firstchoice..)
interpretStatement (Find sel) = do
    ptr <- gets uiPointer
    case findDown (isSelected sel.toTree) ptr of
        (x:_) -> setPointer' x >> return Success
        _     -> return $ Error "Nothing found"


interpretStatement IndicateAuto = do
    ptr <- gets uiPointer
    case explorePointer ptr of
      Left e  -> return $ Error e
      Right t -> modify (\x -> x{uiAutoPointer = Just t}) >> return Success


interpretStatement ShowPlan = (return . Progress . showAssignment . ptrToAssignment) =<< gets uiPointer

interpretStatement (Prefer sel) = do
    modifyPointer (\x -> preferSelections sel `liftToPtr` x)
    return Success


interpretStatement Back = do
    history <- gets uiHistory
    case history of
        (_:reminder) -> do _ <- interpretStatements (Statements (ToTop : reverse reminder)) --TODO: simply discard the return values?
                           modify (\x -> x{uiHistory = reminder})
                           return Success
        _            -> return $ Error "Nothing to go back to"

interpretStatement ShowHistory = do history <- gets uiHistory
                                    return $ Progress $ show history

interpretStatement WhatWorks =
    do ptr <- gets uiPointer
       return                  $
          Progress             $
          show                 $
          map showChild        $
          filter (works ptr)   $
          fromJust             $
          children ptr
    where
      isRight :: Either a b -> Bool
      isRight (Right _) = True
      isRight _         = False
      focus :: QPointer -> ChildType -> QPointer
      focus ptr ch = fromJust $ focusChild ch ptr
      works :: QPointer -> ChildType -> Bool
      works ptr ch = isRight $ explorePointer $ focus ptr ch



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
