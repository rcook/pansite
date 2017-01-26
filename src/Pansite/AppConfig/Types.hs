{-|
Module      : Pansite.Config.Types
Description : Application configuration types for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.AppConfig.Types
    ( AppConfig (..)
    , BuildTool (..)
    , Route (..)
    , Target (..)
    ) where

data AppConfig = AppConfig [Route] [Target] deriving Show
data BuildTool = Copy | Pandoc deriving Show
data Route = Route [String] FilePath deriving Show
data Target = Target FilePath BuildTool [FilePath] deriving Show
