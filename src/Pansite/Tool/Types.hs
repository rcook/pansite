{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Tool.Types
    ( Tool (..)
    , ToolName
    , ToolRunner (..)
    , ToolRunnerMap (..)
    ) where

import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Yaml

type ToolName = String

type ToolRunner = FilePath -> FilePath -> FilePath -> IO ()

type ToolRunnerMap = HashMap String ToolRunner

data Tool = forall a. Default a => Tool ToolName (Value -> Parser a) (a -> ToolRunner)
