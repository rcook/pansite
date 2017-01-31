-- TODO: Consider moving all of this into AppConfig modules
{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Tool.Types
    ( Tool (..)
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

data Tool = forall a. Default a => Tool
    ToolName
    ((FilePath -> FilePath) -> Value -> Parser a)
    (a -> ToolRunner)
