module CopyTool.Instances () where

import           CopyTool.Types
import           Data.Default

instance Default CopySettings where
    def = CopySettings
