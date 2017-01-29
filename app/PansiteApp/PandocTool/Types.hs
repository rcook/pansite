module PansiteApp.PandocTool.Types
    ( PandocSettings (..)
    , PandocVariable
    ) where

import           Data.Default

type PandocVariable = (String, String)

data PandocSettings = PandocSettings
    (Maybe FilePath)
    [PandocVariable]
    deriving Show
