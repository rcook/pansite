module PansiteApp.PandocBuildTool.Types
    ( PandocSettings (..)
    , PandocSettings2 (..)
    , PandocVariable
    ) where

import           Data.Default

type PandocVariable = (String, String)

data PandocSettings = PandocSettings
    { pandocSettingsTemplate :: Maybe FilePath
    } deriving Show

data PandocSettings2 = PandocSettings2
    (Maybe FilePath)
    [PandocVariable]
    deriving Show
