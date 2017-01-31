{-|
Module      : Pansite.Tool.Types
Description : Types for Pansite external tool support
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

-- TODO: Consider moving all of this into AppConfig modules
{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Tool.Types
    ( FilePathResolver
    , ParserContext (..)
    , Tool (..)
    , ToolConfigUpdater
    , ToolConfigRunner
    , ToolContext (..)
    , ToolName
    , ToolRunner (..)
    , ToolRunnerMap (..)
    , ToolSpec (..)
    ) where

import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Yaml

data ToolContext = ToolContext
    { toolContextOutputPath :: FilePath
    , toolContextInputPaths :: [FilePath]
    , toolContextDependencyPaths :: [FilePath]
    }

type ToolName = String

type ToolRunner = ToolContext -> IO ()

type ToolRunnerMap = HashMap String ToolRunner

--type FilePathResolver = FilePath -> FilePath

data Tool = forall a. Default a => Tool
    ToolName
    (FilePathResolver -> Value -> Parser a)
    (a -> ToolRunner)

-- NEW

type FilePathResolver = FilePath -> FilePath

type ToolConfigUpdater a = ParserContext -> a -> Value -> Parser a

type ToolConfigRunner a = ToolContext -> a -> IO ()

data ParserContext = ParserContext FilePathResolver

data ToolSpec = forall a. Default a => ToolSpec
    String                  -- Key
    (ToolConfigUpdater a)   -- Updater function
    (ToolConfigRunner a)    -- Runner function

-- ALSO NEW
