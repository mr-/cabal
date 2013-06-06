module Distribution.Client.Interactive where

import System.Console.Haskeline (outputStrLn, getInputLine, runInputT, 
                                 defaultSettings, InputT)

import Distribution.Client.Dependency.Modular.TreeZipper
        (Pointer(..), ChildType(..), fromTree, toTree,  children, focusChild, focusUp, isRoot, focusRoot)


import Distribution.Client.Dependency.Modular.Dependency 
                (QGoalReasonChain, GoalReason(..), OpenGoal(..), showOpenGoal)


import Distribution.Client.Dependency.Modular.Tree (Tree(..))

import Distribution.Client.Interactive.Parser

import Data.Maybe (fromJust, fromMaybe)

import Distribution.Client.Dependency.Modular.Package(I(..), Loc(..), showQPN, showPI)
import Distribution.Client.Dependency.Modular.Flag(showQSN, showQFN)
import Distribution.Client.Dependency.Modular.Version (showVer)

runInteractive :: Tree QGoalReasonChain -> IO ()
runInteractive searchTree = do 
    putStrLn "Welcome to cabali!"
    runInputT defaultSettings (loop $ Just $ fromTree searchTree)
  where 
        loop :: Maybe (Pointer QGoalReasonChain) -> InputT IO ()
        loop Nothing = outputStrLn "Bye bye"
        loop (Just treePointer) = do
            outputStrLn $ "Node: " ++ ( showNodeFromTree $ toTree treePointer )
            outputStrLn "Choices: "
            outputStrLn $ showChoices treePointer
            tP <- handleCommand treePointer
            loop tP

showChoices :: Pointer QGoalReasonChain -> String
showChoices treePointer = unlines $ map (\(x,y) -> "(" ++ show x ++ ")  " ++ showChild y) $ generateChoices treePointer

--data ChildType = CTP I | CTF Bool | CTS Bool | CTOG OpenGoal deriving (Show)
--data I = I Ver Loc
--data Loc = Inst PId | InRepo
--data OpenGoal = OpenGoal (FlaggedDep QPN) QGoalReasonChain


showChild :: ChildType -> String
showChild (CTP (I ver (Inst _))) = "Version " ++ (showVer ver) ++ "\t(Installed)"
showChild (CTP (I ver (InRepo))) = "Version " ++ (showVer ver)
showChild (CTF bool) = show bool
showChild (CTS bool) = show bool
showChild (CTOG opengoal) = "OpenGoal: " ++ showOpenGoal opengoal
--showChild (CTOG (OpenGoal flagged qgoalreasonchain)) = "OpenGoal: " ++ (show flagged) 
--                                        ++ " Reason: " ++ (show $ head qgoalreasonchain) 


generateChoices :: Pointer a -> [(Int, ChildType)]
generateChoices treePointer = zip [1..] (fromMaybe [] $ children treePointer)

handleCommand :: Pointer QGoalReasonChain -> InputT IO (Maybe (Pointer QGoalReasonChain))
handleCommand  treePointer = do
  inp <- getInputLine "> "
  case inp of
    Nothing -> return Nothing 
    Just text  -> case readExpr text >>= \cmd -> interpretExpression cmd treePointer of
                          Left s  -> do outputStrLn s
                                        handleCommand treePointer 
                          Right t -> return (Just t)


interpretExpression :: Expression -> Pointer QGoalReasonChain ->  Either String (Pointer QGoalReasonChain)
interpretExpression [] _ = error "Internal Error in interpretExpression"
interpretExpression [cmd] treePos = interpretCommand treePos cmd
interpretExpression (x:xs) treePos = interpretCommand treePos x >>= interpretExpression xs

interpretCommand :: Pointer QGoalReasonChain -> Command -> Either String (Pointer QGoalReasonChain)
interpretCommand treePointer ToTop = Right $ focusRoot treePointer
interpretCommand treePointer Up | isRoot treePointer  = Left "We are at the top"
interpretCommand treePointer Up = Right $ fromJust $ focusUp treePointer

interpretCommand treePointer (Go n) = case focused of
                                                Nothing -> Left "No such child"
                                                Just subPointer -> Right subPointer
            where focused = lookup n choices >>= \foo -> focusChild foo treePointer
                  choices = generateChoices treePointer

interpretCommand _ Auto = Left "auto is not implemented yet."




showNodeFromTree :: Tree QGoalReasonChain -> String
showNodeFromTree (PChoice qpn a _)           = "PChoice: QPN: " ++ (showQPN qpn) ++ "\n\t QGoalReason: " ++ (showGoalReason a)
showNodeFromTree (FChoice qfn a b1 b2 _)     = "FChoice: QFN: " ++ (showQFN qfn) ++ "\n\t QGoalReason: " ++ (showGoalReason a) 
                                                    ++ "\n\t Bools: " ++ (show (b1, b2))
showNodeFromTree (SChoice qsn a b _)         = "SChoice: QSN: " ++ (showQSN qsn) ++ "\n\t QGoalReason: " ++ (showGoalReason a) 
                                                    ++ "\n\t Bool " ++ (show b)
showNodeFromTree (GoalChoice _)              = "GoalChoice"
showNodeFromTree (Done rdm)                  = "Done RevDepMap " ++ (show rdm)
showNodeFromTree (Fail cfs fr)               = "Fail " ++ (show (cfs, fr))



{-
type QGoalReason = GoalReason QPN
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show)

data PI qpn = PI qpn I
data I = I Ver Loc

data FN qpn = FN (PI qpn) Flag
-- | Flag identifier. Just a string.
type Flag = FlagName


data SN qpn = SN (PI qpn) OptionalStanza
-}

showGoalReason :: QGoalReasonChain -> String
showGoalReason ((PDependency piqpn ):_) = "PDependency (depended by): " ++ (showPI piqpn)
showGoalReason ((FDependency fnqpn b):_) = "FDependency: " ++ showQFN fnqpn  ++ " Bool: " ++ show b
showGoalReason ((SDependency snqpn):_) = "SDependency " ++ showQSN snqpn
showGoalReason (UserGoal:_) = "UserGoal"
showGoalReason [] = error "Empty QGoalReasonChain - this should never happen, I think"

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