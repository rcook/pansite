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
    { toolContextAppDir :: FilePath
    , toolContextInputPath :: FilePath
    , toolContextOutputPath :: FilePath
    } deriving Show

type ToolName = String

type ToolRunner = ToolContext -> IO ()

type ToolRunnerMap = HashMap String ToolRunner

data Tool = forall a. Default a => Tool ToolName (Value -> Parser a) (a -> ToolRunner)
