module PandocBuildTool.Types
    ( PandocSettings (..)
    ) where

data PandocSettings = PandocSettings
    { pandocSettingsTemplate :: Maybe FilePath
    } deriving Show
