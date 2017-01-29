{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool.Instances () where

import           Control.Applicative
import           Data.Default
import           Data.Text (Text (..))
import           Data.Yaml
import           PansiteApp.PandocTool.Types

instance Default PandocSettings where
    def = PandocSettings Nothing []
