{-|
Module      : Pansite.Config.Types
Description : Types for Pansite app configuration
Copyright   : (C) Richard Cook, 2017-2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Config.Types
    ( App (..)
    , FilePathResolver
    , Route (..)
    , RunContext (..)
    , Target (..)
    , Tool (..)
    , UpdateContext (..)
    ) where

import           Data.Yaml
import           Pansite.PathPattern

type FilePathResolver = FilePath -> FilePath

data UpdateContext = UpdateContext
    FilePathResolver        -- file path resolver

data RunContext = RunContext
    FilePath                -- output path
    [FilePath]              -- input paths
    [FilePath]              -- dependency paths

data Tool = Tool
    String                                  -- key
    (UpdateContext -> Value -> Parser Tool) -- update function
    (RunContext -> IO ())                   -- run function

data App = App [Route] [Target]

data Route = Route [String] FilePath

data Target = Target PathPattern Tool [PathPattern] [PathPattern]
