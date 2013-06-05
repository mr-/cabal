module Distribution.Client.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT, 
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), fromTree, ChildType, children, focusChild, focusUp, isRoot)


import Distribution.Client.Dependency.Modular.Dependency 
                (QGoalReasonChain)

import Distribution.Client.Dependency.Modular.Tree (Tree)

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
interpretCommand _ ToTop _ = undefined
interpretCommand treePointer Up _ | isRoot treePointer  = Left "We are at the top"
interpretCommand treePointer Up _ = Right $ fromJust $ focusUp treePointer

interpretCommand treePointer (Go n) choices = case focused of
                                                Nothing -> Left "No such child"
                                                Just subPointer -> Right subPointer
            where focused = lookup n choices >>= \foo -> focusChild foo treePointer

interpretCommand _ (Auto _) _ = undefined