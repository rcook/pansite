{-|
Module      : PansiteApp.CopyTool.Instances
Description : Copy type class instances
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.CopyTool.Instances () where

import           Data.Default
import           PansiteApp.CopyTool.Types

instance Default CopySettings where
    def = CopySettings
