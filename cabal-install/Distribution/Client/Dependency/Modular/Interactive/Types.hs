module Distribution.Client.Dependency.Modular.Interactive.Types where

import Control.Monad.State                                       (StateT)
import Distribution.Client.Dependency.Modular.Interactive.Parser (Statement)
import Distribution.Client.Dependency.Types                      (QPointer)
import System.Console.Haskeline                                  (InputT)

data UIState = UIState {uiPointer     :: QPointer,
                        uiBookmarks   :: [(String, QPointer)],
                        uiInstall     :: Maybe QPointer,
                        uiAutoPointer :: Maybe QPointer,
                        uiHistory     :: [Statement]}

-- Better uiBreakPoints :: [(String, QPointer -> Bool)]
-- features: cut
-- Figure out what the given bools for flags and Stanzas mean.



type AppState = StateT UIState (InputT IO)

data Action = InstallNow | Abort | Continue | First deriving (Eq)

data UICommand = Error String | ShowChoices | ShowResult String | DoInstall deriving (Show, Eq)


