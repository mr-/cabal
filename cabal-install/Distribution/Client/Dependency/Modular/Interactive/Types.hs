module Distribution.Client.Dependency.Modular.Interactive.Types where

import Distribution.Client.Dependency.Modular.Interactive.Parser (Statement)
import Distribution.Client.Dependency.Types                      (QPointer)

data UIState = UIState {uiPointer         :: QPointer,
                        uiBookmarks       :: [(String, QPointer)],
                        uiInstall         :: Maybe QPointer,
                        uiAutoPointer     :: Maybe QPointer,
                        uiHistory         :: [(Statement, QPointer)]}
                          deriving (Eq)


data Action = InstallNow | Abort | Continue | First deriving (Eq)

data UICommand = Error String | ShowChoices UIState | ShowResult String | DoInstall deriving (Eq)


