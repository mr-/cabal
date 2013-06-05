module Distribution.Client.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT, 
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), fromTree, toTree, ChildType, children, focusChild, focusUp, isRoot)


import Distribution.Client.Dependency.Modular.Dependency 
                (QGoalReasonChain)

import Distribution.Client.Dependency.Modular.Tree (Tree(..))

import Distribution.Client.Interactive.Parser

import Data.Maybe (fromJust, fromMaybe)

type Choices = [(Int, ChildType)]

runInteractive :: Tree QGoalReasonChain -> IO ()
runInteractive searchTree = do 
    putStrLn "Welcome to cabali!"
    runInputT defaultSettings (loop $ Just $ fromTree searchTree)
  where 
        loop :: Maybe (Pointer QGoalReasonChain) -> InputT IO ()
        loop Nothing = outputStrLn "Bye bye"
        loop (Just treePointer) = do
            let choices = zip [1..] (fromMaybe [] $ children treePointer)
            outputStrLn $ "Node: " ++ ( showNodeFromTree $ toTree treePointer )
            outputStrLn "Choices: "
            outputStrLn $ unlines $ map show choices
            tP <- handleCommand treePointer choices
            loop tP




handleCommand :: Pointer QGoalReasonChain -> Choices -> InputT IO (Maybe (Pointer QGoalReasonChain))
handleCommand  treePointer choices = do
  inp <- getInputLine "> "
  case inp of
    Nothing -> return Nothing 
    Just text  -> case readExpr text >>= \cmd -> interpretExpression cmd treePointer choices of
                          Left s  -> do outputStrLn s
                                        handleCommand treePointer choices
                          Right t -> return (Just t)


interpretExpression :: Expression -> Pointer QGoalReasonChain -> Choices -> Either String (Pointer QGoalReasonChain)
interpretExpression [] _ _ = error "Internal Error in interpretExpression"
interpretExpression [cmd] treePos choices = interpretCommand treePos cmd choices
interpretExpression (x:xs) treePos choices = interpretCommand treePos x choices >>= (\t -> interpretExpression xs t choices)

interpretCommand :: Pointer QGoalReasonChain -> Command -> Choices -> Either String (Pointer QGoalReasonChain)
interpretCommand _ ToTop _ = Left "top is not implemented yet."
interpretCommand treePointer Up _ | isRoot treePointer  = Left "We are at the top"
interpretCommand treePointer Up _ = Right $ fromJust $ focusUp treePointer

interpretCommand treePointer (Go n) choices = case focused of
                                                Nothing -> Left "No such child"
                                                Just subPointer -> Right subPointer
            where focused = lookup n choices >>= \foo -> focusChild foo treePointer

interpretCommand _ Auto _ = Left "auto is not implemented yet."




showNodeFromTree :: (Show a) =>  Tree a -> String
showNodeFromTree (PChoice qpn a _)           = "PChoice: QPN: " ++ (show qpn) ++ "\n\t QGoalReasonChain: " ++ (show a)
showNodeFromTree (FChoice qfn a b1 b2 _)     = "FChoice: QFN: " ++ (show qfn) ++ "\n\t QGoalReasonChain: " ++ (show a) 
                                                    ++ "\n\t Bools: " ++ (show (b1, b2))
showNodeFromTree (SChoice qsn a b _)         = "SChoice: QSN: " ++ (show qsn) ++ "\n\t QGoalReasonChain: " ++ (show a) 
                                                    ++ "\n\t Bool " ++ (show b)
showNodeFromTree (GoalChoice _)              = "GoalChoice"
showNodeFromTree (Done rdm)                  = "Done RevDepMap " ++ (show rdm)
showNodeFromTree (Fail cfs fr)               = "Fail " ++ (show (cfs, fr))

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