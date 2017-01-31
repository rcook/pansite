module PansiteApp.PandocTool.Types
    ( PandocSettings (..)
    , PandocVariable
    ) where

type PandocVariable = (String, String)

data PandocSettings = PandocSettings
    (Maybe FilePath)
    [PandocVariable]
    Bool
    deriving Show
