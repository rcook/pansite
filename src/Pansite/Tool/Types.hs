{-# LANGUAGE ExistentialQuantification #-}

module Pansite.Tool.Types
    ( Tool (..)
    , ToolRunner (..)
    , ToolRunnerMap (..)
    ) where

import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Yaml

type ToolRunner = String -> String

type ToolRunnerMap = HashMap String ToolRunner

data Tool = forall a. Default a => Tool String (Value -> Parser a) (a -> ToolRunner)
