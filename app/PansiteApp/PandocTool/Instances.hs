module PansiteApp.PandocTool.Instances () where

import           Data.Default
import           PansiteApp.PandocTool.Types

instance Default PandocSettings where
    def = PandocSettings Nothing []
