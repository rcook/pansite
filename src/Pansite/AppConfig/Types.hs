{-|
Module      : Pansite.Config.Types
Description : Application configuration types for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.AppConfig.Types () where

{-
data AppConfig = AppConfig [Route] [Target] ToolRunnerMap
data Route = Route [String] FilePath deriving Show
data Target = Target
    { targetPath :: FilePath
    , targetTool :: ToolName
    , targetInputs :: [FilePath]
    , targetDependencies :: [FilePath]
    }
-}
