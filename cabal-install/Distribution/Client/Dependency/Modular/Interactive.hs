module Distribution.Client.Dependency.Modular.Interactive where

import Control.Applicative                                            ((<$>), (<*>))
import Control.Monad.State                                            (evalStateT, get, gets, lift)
import Data.List                                                      (isPrefixOf)
import Distribution.Client.Dependency                                 (DepResolverParams,
                                                                       resolveDependenciesConfigs)
import Distribution.Client.Dependency.Modular                         (modularResolverTree)
import Distribution.Client.Dependency.Modular.Interactive.Interpreter (generateChoices,
                                                                       interpretStatements)
import Distribution.Client.Dependency.Modular.Interactive.Parser      (Statements (..), commandList,
                                                                       readStatements)
import Distribution.Client.Dependency.Modular.Interactive.Types       (Action (..), AppState,
                                                                       UICommand (..), UIState (..))
import Distribution.Client.Dependency.Modular.Tree                    (ChildType (..), Tree (..),
                                                                       isInstalled, showChild)
import Distribution.Client.Dependency.Modular.Message                 (showNodeFromTree)
import Distribution.Client.Dependency.Modular.TreeZipper              (Pointer (..), focusChild,
                                                                       fromTree, pointsBelow,
                                                                       toTree)
import Distribution.Client.Dependency.Types                           (QPointer, Solver (..))
import Distribution.Simple.Compiler                                   (CompilerId)
import Distribution.System                                            (Platform)
import System.Console.Haskeline                                       (completeWord,
                                                                       defaultSettings,
                                                                       getInputLine, outputStrLn,
                                                                       runInputT, setComplete)
import System.Console.Haskeline.Completion                            (Completion (..),
                                                                       CompletionFunc)
runInteractive :: Platform
               -> CompilerId
               -> Solver
               -> DepResolverParams
               -> IO (Maybe QPointer)
runInteractive platform compId solver resolverParams = do
    let (sc, depResOpts) = resolveDependenciesConfigs platform compId solver resolverParams
        searchTree       = modularResolverTree sc depResOpts
        initialState     = UIState (fromTree searchTree) [] Nothing Nothing []

    putStrLn "\n\tWelcome to cabali!"
    putStrLn "\tType \"help\" for help.\n"

    runLoop initialState
      where
        runLoop :: UIState -> IO (Maybe QPointer)
        runLoop s = runInputT completion $ evalStateT (loop First) s

        loop :: Action -> AppState (Maybe QPointer)
        loop Abort = do
          lift $ outputStrLn "Bye bye"
          return Nothing

        loop InstallNow = do
          toInstall <- gets uiInstall
          lift $ outputStrLn "Bye bye"
          return toInstall

        loop First = do
          _ <- executeUICommand ShowChoices
          action <- handleCommands
          loop action

        loop Continue = do
          action <- handleCommands
          loop action


        completion = setComplete cmdComplete defaultSettings

        cmdComplete :: CompletionFunc IO
        cmdComplete = completeWord Nothing " " completions
          where
            completions :: String -> IO [Completion]
            completions str = return $ map (\f -> Completion f f False) (x str)
            x :: String -> [String]
            x str          = filter (isPrefixOf str) commandList

handleCommands :: AppState Action
handleCommands = do
  inp <- lift $ getInputLine "> "
  case inp of
    Nothing    -> return Abort
    Just text  -> case readStatements text of
                    Left s     -> do lift $ outputStrLn s
                                     handleCommands
                    Right cmds -> handleCommands' cmds
  where
    handleCommands' :: Statements -> AppState Action
    handleCommands' cmds = do
      uiCommands <- interpretStatements cmds
      r <- mapM executeUICommand uiCommands
      return (if InstallNow `elem` r then InstallNow else Continue)


executeUICommand :: UICommand -> AppState Action
executeUICommand (Error s)      = do
    lift $ outputStrLn s
    return Continue
executeUICommand (ShowChoices)  = do
    uiState <- get
    lift $ outputStrLn $ showNodeFromTree ( toTree $ uiPointer uiState )
    lift $ outputStrLn $ displayChoices uiState `thisOrThat` "No choices left"
    return Continue
  where
    thisOrThat :: String -> String -> String
    "" `thisOrThat` s = s
    s  `thisOrThat` _ = s
executeUICommand (ShowResult s) = do
    lift $ outputStrLn s
    return Continue
executeUICommand (DoInstall) = return InstallNow


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
          (Just (Pointer _ (Fail _ _ _))) -> True
          _                               -> False

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
