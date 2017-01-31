{-|
Module      : PansiteApp.PandocTool.Types
Description : Types for Pandoc tool
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

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
