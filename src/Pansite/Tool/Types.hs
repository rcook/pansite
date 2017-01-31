-- TODO: Consider moving all of this into AppConfig modules
{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Tool.Types
    ( FilePathResolver
    , Tool (..)
    , ToolContext (..)
    , ToolName
    , ToolRunner (..)
    , ToolRunnerMap (..)
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

type FilePathResolver = FilePath -> FilePath

data Tool = forall a. Default a => Tool
    ToolName
    (FilePathResolver -> Value -> Parser a)
    (a -> ToolRunner)
