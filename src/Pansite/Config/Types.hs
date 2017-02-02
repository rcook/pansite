{-|
Module      : Pansite.Config.Types
Description : Types for Pansite app configuration
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Config.Types
    ( App (..)
    , FilePathResolver
    , ParserContext (..)
    , Route (..)
    , Target (..)
    , ToolConfig (..)
    , ToolConfigRunner
    , ToolConfigUpdater
    , ToolContext (..)
    , ToolSpec (..)
    ) where

import           Data.Default
import           Data.Yaml

type FilePathResolver = FilePath -> FilePath

type ToolConfigUpdater a = ParserContext -> a -> Value -> Parser a

type ToolConfigRunner a = ToolContext -> a -> IO ()

data ParserContext = ParserContext
    FilePathResolver        -- file path resolver

data ToolContext = ToolContext
    FilePath                -- output path
    [FilePath]              -- input paths
    [FilePath]              -- dependency paths

data ToolSpec = forall a. Default a => ToolSpec
    String                  -- key
    (ToolConfigUpdater a)   -- updater function
    (ToolConfigRunner a)    -- runner function

data App = App [Route] [Target]

data Route = Route [String] FilePath

data Target = Target FilePath ToolConfig [FilePath] [FilePath]

data ToolConfig = forall a. ToolConfig (ToolConfigUpdater a) (ToolConfigRunner a) a