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
    { toolContextMakeTargetPath :: FilePath -> FilePath -- TODO: Glorious hack!
    , toolContextInputPath :: FilePath
    , toolContextOutputPath :: FilePath
    }

type ToolName = String

type ToolRunner = ToolContext -> IO ()

type ToolRunnerMap = HashMap String ToolRunner

data Tool = forall a. Default a => Tool ToolName (Value -> Parser a) (a -> ToolRunner)
