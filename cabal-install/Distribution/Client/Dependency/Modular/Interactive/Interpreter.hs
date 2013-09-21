module Distribution.Client.Dependency.Modular.Interactive.Interpreter where

import Control.Applicative                                       ((<$>))
import Control.Monad                                             (when)
import Control.Monad.State                                       (State, gets, modify)
import Data.Function                                             (on)
import Data.Char                                                 (toLower)
import Data.List                                                 (find)
import Data.Maybe                                                (fromJust, fromMaybe)
import Distribution.Client.Dependency.Modular.Assignment         (showAssignment)
import Distribution.Client.Dependency.Modular.Dependency         (Dep (..), FlaggedDep (..),
                                                                  OpenGoal (..))
import Distribution.Client.Dependency.Modular.Explore            (intermediateAssignment,
                                                                  ptrToAssignment)
import Distribution.Client.Dependency.Modular.Flag               (unQFN, unQSN)
import Distribution.Client.Dependency.Modular.Interactive.Parser (Selection (..), Selections (..),
                                                                  Statement (..), Statements (..), GoChoice (..))
import Distribution.Client.Dependency.Modular.Interactive.Types  (UICommand (..),
                                                                  UIState (..))
import Distribution.Client.Dependency.Modular.Package            (QPN, showQPN)
import Distribution.Client.Dependency.Modular.PSQ                (toLeft)
import Distribution.Client.Dependency.Modular.Solver             (explorePointer)
import Distribution.Client.Dependency.Modular.Tree               (ChildType (..), Tree (..),
                                                                  TreeF (..), showChild, trav)
import Distribution.Client.Dependency.Modular.TreeZipper         (Pointer (..), children,
                                                                  filterBetween, filterDown,
                                                                  focusChild, focusRoot, focusUp,
                                                                  liftToPtr, toTree)
import Distribution.Client.Dependency.Types                      (QPointer)
import Distribution.Client.Dependency.Modular.CompactTree

type InterpreterState = State UIState

interpretStatement :: Statement -> InterpreterState [UICommand]
interpretStatement ToTop = modifyPointer focusRoot >> return [ShowChoices]

interpretStatement Up = do
  ptr <- gets uiPointer
  setPointer (fromMaybe ptr (focusUp ptr))
  return [ShowChoices]

interpretStatement (Go there) = do
  treePointer <- gets uiPointer
  let choices = generateChoices treePointer
      focused = select there choices >>= flip focusChild treePointer
  case focused of
    Nothing -> return [Error $ "No such child: " ++ show there]
    Just subPointer -> do setPointer subPointer
                          return [ShowChoices]
    where
        select (Number n) choices  = lookup (fromInteger n) choices
        select (Version x) choices = snd <$> find (\(_,c) -> x == showChild c) choices
        select (Package x) choices = snd <$> find (\(_,c) -> x == showChild c) choices

interpretStatement Empty = interpretStatement (Go (Number 1))

interpretStatement Auto = do
  pointer <- gets uiPointer
  case explorePointer pointer of
    Left e  -> return [Error e]
    Right t -> setPointer t >> return [ShowResult "Created a valid installplan. \nType install to install, or showPlan to review" ]

interpretStatement (BookSet name) = addBookmark name >> return [ShowResult (name ++ " set")]
  where
    addBookmark :: String -> InterpreterState ()
    addBookmark s = modify (\u -> u {uiBookmarks = (s, uiPointer u) : uiBookmarks u})

interpretStatement BookList = do bookmarks <- gets uiBookmarks
                                 return [ShowResult $ show $ map fst bookmarks]

interpretStatement (BookJump name) = do
    bookmarks <- gets uiBookmarks
    case lookup name bookmarks of
        Nothing -> return [Error "No such bookmark."]
        Just a  -> setPointer a >> return [ShowResult "Jumped"] -- Actual progress!


-- TODO: Should the selected packages get precedence? Or should they be
-- traversed in the given order? (order makes a difference in selection,
-- e.g. when installing cabal-install, goto zlib has fewer options than
-- selecting it manually as early as possible.)
interpretStatement (Goto selections) = do
    pointer <- gets uiPointer
    case explorePointer pointer of
        Left e  -> return [Error e]
        Right t -> do
          let newPointer = selectPointer selections pointer t
              assignment = intermediateAssignment pointer newPointer
          setPointer newPointer
          return [ShowResult $ showAssignment assignment, ShowChoices]
    where
      selectPointer :: Selections -> QPointer -> QPointer -> QPointer
      selectPointer sel here done = last $ done:found
       where
          found = filterBetween (isSelected sel . toTree) here done

interpretStatement Install = do ptr <- gets uiPointer
                                if isDone ptr
                                 then modify (\x -> x{uiInstall = Just ptr}) >> return [DoInstall]
                                 else return [Error "Need to be on a \"Done\"-Node"]

interpretStatement (Cut _) = return [Error "Ooops.. not implemented yet."]

 -- Need to make that more efficient, prune tree first. (firstchoice..)
interpretStatement (Find sel) = do
    ptr <- gets uiPointer
    case filterDown (isSelected sel.toTree) ptr of
        (x:_) -> setPointer x >> return [ShowChoices]
        _     -> return [Error "Nothing found"]


interpretStatement IndicateAuto = do
    ptr <- gets uiPointer
    case explorePointer ptr of
      Left e  -> return [Error e]
      Right t -> modify (\x -> x{uiAutoPointer = Just t}) >> return [ShowChoices]


interpretStatement ShowPlan = do
    ptr <- gets uiPointer
    return [ShowResult $ showAssignment $ ptrToAssignment ptr]

interpretStatement (Prefer sel) = do
    modifyPointer (\x -> preferSelections sel `liftToPtr` x)
    return [ShowChoices]


interpretStatement Back = do
    history <- gets uiHistory
    case history of
        ((s,oldPtr):reminder) -> do when (s == IndicateAuto) (modify (\x -> x{uiAutoPointer = Nothing}))
                                    setPointer oldPtr
                                    modify (\x -> x{uiHistory = reminder})
                                    return [ShowChoices]
        _                     -> return [Error "Nothing to go back to"]

interpretStatement ShowHistory = do history <- gets uiHistory
                                    return [ShowResult $ show $ map fst history]

interpretStatement WhatWorks =
    do ptr <- gets uiPointer
       return                  [
          ShowResult           $
          show                 $
          map showChild        $
          filter (works ptr)   $
          fromJust             $
          children ptr         ]
    where
      isRight :: Either a b -> Bool
      isRight (Right _) = True
      isRight _         = False
      focus :: QPointer -> ChildType -> QPointer
      focus ptr ch = fromJust $ focusChild ch ptr
      works :: QPointer -> ChildType -> Bool
      works ptr ch = isRight $ explorePointer $ focus ptr ch

interpretStatement (Reason) = do
    ptr <- gets uiPointer
    case doBFS (toTree ptr) of
      Nothing               -> return [ShowResult "Uhoh.. got Nothing, call me!"]
      (Just (_, True))      -> return [ShowResult "Found a solution - cannot find a reason."]
      (Just (path, False))  -> return [ShowResult $ take 800 $ showPaths (toTree ptr), ShowResult (show $ map showCOpenGoal path)]


interpretStatement Help = return [ShowResult helpText, ShowChoices]

isDone :: QPointer -> Bool
isDone (Pointer _ (Done _ )  ) = True
isDone _                       = False

isFail :: QPointer -> Bool
isFail (Pointer _ (Fail _  _ _) )  = True
isFail _                           = False


isSelected :: Selections -> Tree a ->  Bool
isSelected (Selections selections) tree  = or [tree `nodeMatches` selection | selection <- selections]
  where
    nodeMatches :: Tree a -> Selection ->  Bool
    nodeMatches (PChoice qpn _ _)     (SelPChoice pname)        =  pname  `matches` showQPN qpn
    nodeMatches (FChoice qfn _ _ _ _) (SelFSChoice name flag)   = (name   `matches` qfnName)
                                                               && (flag   `matches` qfnFlag)
      where (qfnName, qfnFlag) = unQFN qfn
    nodeMatches (SChoice qsn _ _ _ )  (SelFSChoice name stanza) = (name   `matches` qsnName)
                                                               && (stanza `matches` qsnStanza)
      where (qsnName, qsnStanza) = unQSN qsn
    nodeMatches _                     _                         = False


matches :: String -> String -> Bool
matches = (==) `on` (map toLower)


preferSelections :: Selections -> Tree a -> Tree a
preferSelections sel = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (toLeft (depMatchesAny sel) xs)
    go x                = x

    depMatchesAny :: Selections -> OpenGoal -> Bool
    depMatchesAny (Selections selections) (OpenGoal fd _) = any (depMatches fd) selections

    depMatches :: FlaggedDep QPN -> Selection -> Bool
    depMatches (Simple (Dep qpn _)) (SelPChoice name)       = name `matches` showQPN qpn
    depMatches (Flagged qfn _ _ _)  (SelFSChoice name flag) = (name `matches` qfnName) && (flag `matches` qfnFlag)
      where (qfnName, qfnFlag) = unQFN qfn
    depMatches (Stanza qsn _)       (SelFSChoice name flag) = (name `matches` qsnName) && (flag `matches` qsnFlag)
      where (qsnName, qsnFlag) = unQSN qsn
    depMatches _                    _                       = False

interpretStatements :: Statements -> InterpreterState [UICommand]
interpretStatements (Statements [])     = return []
interpretStatements (Statements (c:md)) = do
      ptr <- gets uiPointer
      result <- interpretStatement c
      case result of
        [Error _ ] -> return result -- return the result and stop processing commands.
        _          -> do addHistory c ptr
                         list   <- interpretStatements (Statements md)
                         return $ result ++ list

-- TODO: Back does not revert indicateAuto, and prefer..
addHistory :: Statement -> QPointer -> InterpreterState ()
addHistory Back      _   = return ()
addHistory statement ptr =
    modify (\uiState -> uiState {uiHistory = (statement,ptr):uiHistory uiState})

setPointer :: QPointer -> InterpreterState ()
setPointer ptr = modifyPointer (const ptr)

modifyPointer :: (QPointer -> QPointer) -> InterpreterState ()
modifyPointer f = do ptr <- gets uiPointer
                     modify (\uiState ->uiState{uiPointer = f ptr})

generateChoices :: QPointer -> [(Int, ChildType)]
generateChoices treePointer = zip [1..] (fromMaybe [] $ children treePointer)



helpText :: String
helpText = unlines [
  "This interface accepts simple commands separated by ';'. E.g. go 1 ; auto",
  "go n            chooses n - alternatively \"n\" does the same. Just Enter, picks the first choice",
  "up              goes up one step",
  "top             goes all the way to the top",
  "auto            starts the automatic solver",
  "goto aeson      runs the solver until it sets aeson's version",
  "goto aeson:test runs the solver until it sets the flag test for aeson",
  "prefer aeson    sorts the choices so that aeson comes first if it is available (Same arguments as goto)",
  "bset name       sets a bookmark called name",
  "blist           lists all bookmarks",
  "bjump name      jumps to the bookmark name",
  "indicateAuto    indicates the choices the solver would have made with a little (*)",
  "install         Once the interface says 'Done', you can type 'install' to install the package",
  "showPlan        shows what is going to be installed/used",
  "whatWorks       lists the choices that lead to a valid installplan",
  "back            goes back to the last command",
  "reason n        gives you the first n reasons (muses) why there is no solution"]


