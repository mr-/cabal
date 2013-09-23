module Distribution.Client.Dependency.Modular.Interactive.Types where

import Distribution.Client.Dependency.Modular.Interactive.Parser (Statement)
import Distribution.Client.Dependency.Types                      (QPointer)

data UIState = UIState {uiPointer         :: QPointer,
                        uiBookmarks       :: [(String, QPointer)],
                        uiInstall         :: Maybe QPointer,
                        uiAutoPointer     :: Maybe QPointer,
                        uiHistory         :: [(Statement, QPointer)]}


data Action = InstallNow | Abort | Continue | First deriving (Eq)

data UICommand = Error String | ShowChoices | ShowResult String | DoInstall deriving (Show, Eq)


