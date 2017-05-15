{-|
Module      : Pansite.Config.Types
Description : Types for Pansite app configuration
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Config.Types
    ( App (..)
    , FilePathResolver
    , ParserContext (..)
    , Route (..)
    , Target (..)
    , ToolConfig (..)
    , ToolContext (..)
    ) where

import           Data.Yaml
import           Pansite.PathPattern

type FilePathResolver = FilePath -> FilePath

data ParserContext = ParserContext
    FilePathResolver        -- file path resolver

data ToolContext = ToolContext
    FilePath                -- output path
    [FilePath]              -- input paths
    [FilePath]              -- dependency paths

data ToolConfig = ToolConfig
    String                                          -- key
    (ParserContext -> Value -> Parser ToolConfig)   -- updater function
    (ToolContext -> IO ())                          -- runner function

data App = App [Route] [Target]

data Route = Route [String] FilePath

data Target = Target PathPattern ToolConfig [PathPattern] [PathPattern]
