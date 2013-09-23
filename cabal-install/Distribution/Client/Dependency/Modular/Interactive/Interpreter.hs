module Distribution.Client.Dependency.Modular.Interactive.Interpreter where

import Control.Applicative                                       ((<$>), liftA2)
import Control.Monad                                             (when)
import Control.Monad.State                                       (State, gets, modify, get)
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
import Distribution.Client.Dependency.Modular.MUS                (findMUS, showPath)
import Prelude hiding (and)

type InterpreterState = State UIState

interpretStatement :: Statement -> InterpreterState [UICommand]
interpretStatement ToTop = modifyPointer focusRoot >> showChoices

interpretStatement Up = do
  ptr <- gets uiPointer
  setPointer (fromMaybe ptr (focusUp ptr))
  showChoices

interpretStatement (Go there) = do
  treePointer <- gets uiPointer
  let choices = generateChoices treePointer
      focused = select there choices >>= flip focusChild treePointer
  case focused of
    Nothing -> showError $ "No such child: " ++ show there
    Just subPointer -> do setPointer subPointer
                          showChoices
    where
        select (Number n) choices  = lookup (fromInteger n) choices
        select (Version x) choices = snd <$> find (\(_,c) -> x == showChild c) choices
        select (Package x) choices = snd <$> find (\(_,c) -> lower x == lower (showChild c)) choices

interpretStatement Empty = interpretStatement (Go (Number 1))

interpretStatement Auto = do
  pointer <- gets uiPointer
  case explorePointer pointer of
    Left e  -> return [Error e]
    Right t -> setPointer t >> showResult "Created a valid installplan. \nType install to install, or showPlan to review"

interpretStatement (BookSet name) = addBookmark name >> showResult (name ++ " set")
  where
    addBookmark :: String -> InterpreterState ()
    addBookmark s = modify (\u -> u {uiBookmarks = (s, uiPointer u) : uiBookmarks u})

interpretStatement BookList = do bookmarks <- gets uiBookmarks
                                 showResult $ show $ map fst bookmarks

interpretStatement (BookJump name) = do
    bookmarks <- gets uiBookmarks
    case lookup name bookmarks of
        Nothing -> showError "No such bookmark."
        Just a  -> setPointer a >> showResult "Jumped" -- Actual progress!


-- TODO: Should the selected packages get precedence? Or should they be
-- traversed in the given order? (order makes a difference in selection,
-- e.g. when installing cabal-install, goto zlib has fewer options than
-- selecting it manually as early as possible.)
interpretStatement (Goto selections) = do
    pointer <- gets uiPointer
    case explorePointer pointer of
        Left e  -> showError e
        Right t -> do
          let newPointer = selectPointer selections pointer t
              assignment = intermediateAssignment pointer newPointer
          setPointer newPointer
          showResult (showAssignment assignment) `and` showChoices
    where
      selectPointer :: Selections -> QPointer -> QPointer -> QPointer
      selectPointer sel here done = last $ done:found
       where
          found = filterBetween (isSelected sel . toTree) here done

interpretStatement Install = do ptr <- gets uiPointer
                                if isDone ptr
                                 then modify (\x -> x{uiInstall = Just ptr}) >> install
                                 else showError "Need to be on a \"Done\"-Node"

interpretStatement (Cut _) = showError "Ooops.. not implemented yet."

 -- Need to make that more efficient, prune tree first. (firstchoice..)
interpretStatement (Find sel) = do
    ptr <- gets uiPointer
    case filterDown (isSelected sel.toTree) ptr of
        (x:_) -> setPointer x >> showChoices
        _     -> showError "Nothing found"


interpretStatement IndicateAuto = do
    ptr <- gets uiPointer
    case explorePointer ptr of
      Left e  -> showError e
      Right t -> modify (\x -> x{uiAutoPointer = Just t}) >> showChoices


interpretStatement ShowPlan = do
    ptr <- gets uiPointer
    showResult $ showAssignment $ ptrToAssignment ptr

interpretStatement (Prefer sel) = do
    modifyPointer (\x -> preferSelections sel `liftToPtr` x)
    showChoices


interpretStatement Back = do
    history <- gets uiHistory
    case history of
        ((s,oldPtr):reminder) -> do when (s == IndicateAuto) (modify (\x -> x{uiAutoPointer = Nothing}))
                                    setPointer oldPtr
                                    modify (\x -> x{uiHistory = reminder})
                                    showChoices
        _                     -> showError "Nothing to go back to"

interpretStatement ShowHistory = do history <- gets uiHistory
                                    showResult $ show $ map fst history

interpretStatement WhatWorks =
    do ptr <- gets uiPointer
       showResult             $
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

interpretStatement (FailReason) = do
    ptr <- gets uiPointer
    case findMUS (toTree ptr) of
      Nothing               -> showResult "Uhoh.. got Nothing, call me!"
      (Just (_, True))      -> showResult "Found a solution - cannot find a failreason."
      (Just (path, False))  -> showResult $ "The following packages contradict each other:\n\t" ++ showPath path
                                                                   -- TODO: In what?
                                    --   ShowResult $ baz (toTree ptr),
                                    --   ShowResult $ take 2000 $ showThinnedPaths (toTree ptr),
                                    --   ShowResult $ take 100 $ showThinnedPathsBFS (toTree ptr),
                                    --   ShowResult $ show $ treeHasDuplicates 3 ( removeDuplicates $ toCompact $ toSimple $ toTree ptr)]


interpretStatement Help = showResult helpText `and` showChoices

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
      where (qfnName, qfnFlag)   = unQFN qfn
    nodeMatches (SChoice qsn _ _ _ )  (SelFSChoice name stanza) = (name   `matches` qsnName)
                                                               && (stanza `matches` qsnStanza)
      where (qsnName, qsnStanza) = unQSN qsn
    nodeMatches _                     _                         = False


matches :: String -> String -> Bool
matches = (==) `on` lower

lower :: String -> String
lower = map toLower

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


showResult :: String -> InterpreterState [UICommand]
showResult str = return [ShowResult str]

showChoices :: InterpreterState [UICommand]
showChoices = get >>= \x -> return [ShowChoices x]

showError :: String -> InterpreterState [UICommand]
showError str = return [Error str]

install :: InterpreterState [UICommand]
install = return [DoInstall]

and :: InterpreterState [UICommand] -> InterpreterState [UICommand] -> InterpreterState [UICommand]
and = liftA2 (++)



helpText :: String
helpText = unlines [
  "This interface accepts simple commands separated by ';'. E.g. go 1 ; auto",
  "\nExample: " ,
  "  go 1.0.4.2; bset foo ; goto aeson | parsec:test ; bjump foo ; auto",
  "                                 ^          ^ this is both, flag or stanza",
  "                                 | while this is just package choice",
  "\nCommands: ",
  "  go n          pick the n'th path. Alternatively, you can just type the packageName ",
  "                or the version. It matches case-insensitively",
  "  up            go up one step",
  "  top           go all the way to the root of the dependencytree",
  "  auto          start the automatic solver and jump to the node corresponding to a valid",
  "                installPlan or print an error if it fails",
  "  goto foo      run the solver until it sets foo's version",
  "                The arguments can have the form 'foo:test'",
  "                or 'foo | bar | baz:test' to go to the first matching choice",
  "                (What it does is: Look for a solution, calculate a path to that solution and",
  "                go to the earliest node matching one of the arguments)",
  "  prefer foo    sort the choices so that foo comes first if it is available. It takes the",
  "                same arguments as goto",
  "  indicateAuto  indicate the choices the solver would have made with a little (*)",
  "  install       once the interface says so, you can type 'install' to install the package(s)",
  "  showPlan      show the assignment up to this point",
  "  whatWorks     list the choices currently available that lead to a valid installPlan",
  "  back          go back to the last command",
  "  failReason    print the first (pseudo-)MUS.",
  "                This lists an almost minimal set of unsatisfiable packages.",
  "                Warning: This may eat all your memory",
  "  bset bar      set a bookmark called 'bar'",
  "  blist         list all bookmarks",
  "  bjump bar     jump to the bookmark 'bar'"]


