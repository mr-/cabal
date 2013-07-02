module Distribution.Client.Dependency.Modular.Interactive where

import Control.Applicative                                       ((<$>), (<*>), (<|>))
import Data.Char                                                 (toLower)
import Data.List                                                 (isInfixOf)
import Data.Maybe                                                (fromJust, fromMaybe, isJust)
import Distribution.Client.Dependency.Modular.Assignment         (showAssignment)
import Distribution.Client.Dependency                            (DepResolverParams,
                                                                  resolveDependenciesConfigs)
import Distribution.Client.Dependency.Modular                    (modularResolverTree)
import Distribution.Client.Dependency.Modular.Dependency         (QGoalReasonChain)
import Distribution.Client.Dependency.Modular.Flag               (unQFN, unQSN)
import Distribution.Client.Dependency.Modular.Interactive.Parser (Selection (..), Selections (..),
                                                                  Statement (..), Statements (..),
                                                                  readStatements)
import Distribution.Client.Dependency.Modular.Package            (showQPN)
import Distribution.Client.Dependency.Modular.Solver             (explorePointer)
import Distribution.Client.Dependency.Modular.Explore            (donePtrToLog, runTreePtrLog)
import Distribution.Client.Dependency.Modular.Tree               (ChildType (..), Tree (..),
                                                                  isInstalled, showChild,
                                                                  showNodeFromTree)
import Distribution.Client.Dependency.Modular.TreeZipper         (Pointer (..), children,
                                                                  filterBetween, findDown,
                                                                  focusChild, focusRoot, focusUp,
                                                                  fromTree, isRoot, pointsBelow,
                                                                  toTree)
import Distribution.Client.Dependency.Types                      (Solver (..))
import Distribution.Simple.Compiler                              (CompilerId)
import Distribution.System                                       (Platform)
import System.Console.Haskeline                                  (InputT, defaultSettings,
                                                                  getInputLine, outputStrLn,
                                                                  runInputT)

data UIState a = UIState {uiPointer     :: Pointer a,
                          uiBookmarks   :: [(String, Pointer a)],
                          uiInstall     :: Maybe (Pointer a),       -- these point
                          uiAutoPointer :: Maybe (Pointer a)}       -- to Done

setAutoPointer :: UIState a -> Pointer a -> UIState a
setAutoPointer state nP = state {uiAutoPointer = Just nP}

setPointer :: UIState a -> Pointer a -> UIState a
setPointer state nP = state {uiPointer = nP}

setInstall :: UIState a -> Pointer a -> UIState a
setInstall state nP = state {uiInstall = Just nP}

isInstall :: UIState a -> Bool
isInstall = isJust.uiInstall

noInstall :: UIState a -> Bool
noInstall = not.isInstall

getInstall :: UIState a -> Pointer a
getInstall uiState = case uiAutoPointer uiState of
                        Just x -> x
                        _      -> error "Outch.. got wrong install"

-- Better uiBreakPoints :: [(String, Pointer a -> Bool)]
-- features: cut
-- Figure out what the given bools for flags and Stanzas mean.

runInteractive :: Platform
               -> CompilerId
               -> Solver
               -> DepResolverParams
               -> IO (Maybe (Pointer QGoalReasonChain))
runInteractive platform compId solver resolverParams = do
    let (sc, depResOpts)     = resolveDependenciesConfigs platform compId solver resolverParams
        searchTree           = modularResolverTree sc depResOpts

    putStrLn "Welcome to cabali!"
    putStrLn "This interface accepts simple commands separated by ';'. E.g. go 1 ; auto"
    putStrLn "go n            chooses n - alternatively \"n\" does the same. Just Enter, picks the first choice"
    putStrLn "up              goes up one step"
    putStrLn "top             goes all the way to the top"
    putStrLn "auto            starts the automatic solver"
    putStrLn "goto aeson      runs the parser until it sets aeson's version"
    putStrLn "goto aeson:test runs the parser until it sets the flag test for aeson"
    putStrLn "bset name       sets a bookmark called name"
    putStrLn "blist           lists all bookmarks"
    putStrLn "bjump name      jumps to the bookmark name"
    putStrLn "indicateAuto    indicates the choices the solver would have made with a little (*)"
    putStrLn "install         Once the interface says 'Done', you can type 'install' to install the package"
    putStrLn "showPlan        shows what is going to be installed/used"

    runInputT defaultSettings (loop $ Just $ UIState (fromTree searchTree) [] Nothing Nothing)
  where
        loop :: Maybe (UIState QGoalReasonChain) -> InputT IO (Maybe (Pointer QGoalReasonChain))
        loop Nothing =
          outputStrLn "Bye bye" >> return Nothing

        loop (Just uiState) | isInstall uiState =
          outputStrLn "Bye bye" >> return (uiInstall uiState)

        loop (Just uiState) = do
          outputStrLn $ showNodeFromTree ( toTree $ uiPointer uiState )
          outputStrLn $ displayChoices uiState `thisOrThat` "No choices left"
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
interpretStatements (Statements [])     _        = error "Internal Error in interpretExpression"
interpretStatements (Statements [cmd])  uiState  = interpretStatement uiState cmd
interpretStatements (Statements (x:xs)) uiState  = interpretStatement uiState x >>= interpretStatements (Statements xs)


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

interpretStatement uiState Empty =  case choices of -- behave differently when indicateAuto is on?
                        ((_, child):_) -> Right $ setPointer uiState $ fromJust $ focusChild child treePointer
                        _              -> Left "No choice left"
  where choices     = generateChoices treePointer
        treePointer = uiPointer uiState


interpretStatement uiState Auto = autoRun uiState

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

interpretStatement uiState ShowPlan | isDone (uiPointer uiState) =
    case runTreePtrLog $ donePtrToLog (uiPointer uiState) of
      Left s                -> Left s
      Right (assignment, _) -> Left $ showAssignment assignment
interpretStatement _ _ = Left "Do not have a plan to show"

isDone :: Pointer a -> Bool
isDone (Pointer _ (Done _ )  ) = True
isDone _                       = False



isSelected :: Selections -> Tree QGoalReasonChain ->  Bool
isSelected (Selections selections) tree  = or [tree `matches` selection | selection <- selections]
  where
    matches :: Tree a -> Selection ->  Bool
    matches (PChoice qpn _ _)     (SelPChoice pname)         =   pname   `isSubOf` showQPN qpn

    matches (FChoice qfn _ _ _ _) (SelFSChoice name flag)    =   (name   `isSubOf` qfnName)
                                                                          && (flag   `isSubOf` qfnFlag)
      where
        (qfnName, qfnFlag) = unQFN qfn
    matches (SChoice qsn _ _ _ )  (SelFSChoice name stanza)  =   (name   `isSubOf` qsnName)
                                                                          && (stanza `isSubOf` qsnStanza)
      where
        (qsnName, qsnStanza) = unQSN qsn
    matches _          _                                 = False

    isSubOf :: String -> String -> Bool
    isSubOf x y = lower x `isInfixOf` lower y
      where
        lower :: String -> String
        lower = map toLower

autoRun :: UIState QGoalReasonChain -> Either String (UIState QGoalReasonChain)
autoRun uiState = setPointer uiState <$> (explorePointer . uiPointer) uiState


displayChoices :: UIState QGoalReasonChain -> String
displayChoices uiState = prettyShow $ map (uncurry makeEntry) $ generateChoices treePointer
  where
    treePointer = uiPointer uiState
    makeEntry :: Int -> ChildType -> String
    makeEntry n child = "(" ++ show n ++ ") " ++ showChild child ++ " " ++ comment child

    -- I am not sure I like this..
    comment :: ChildType -> String
    comment child = case str of
                [] -> ""
                x  -> "(" ++ x ++ ")"
      where
        str = concatMap (\f -> f child) [autoComment, installComment, failComment]

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
