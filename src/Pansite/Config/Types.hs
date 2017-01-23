{-|
Module      : Pansite.Config.Types
Description : Configuration types for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Config.Types
    ( BuildTool (..)
    , Config (..)
    , Route (..)
    , Target (..)
    ) where

data BuildTool = Pandoc deriving Show
data Config = Config [Route] [Target] deriving Show
data Route = Route [String] FilePath deriving Show
data Target = Target FilePath BuildTool [FilePath] deriving Show
