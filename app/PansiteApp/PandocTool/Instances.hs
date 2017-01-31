{-|
Module      : PansiteApp.PandocTool.Instances
Description : Pandoc type class instances
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.PandocTool.Instances () where

import           Data.Default
import           PansiteApp.PandocTool.Types

instance Default PandocSettings where
    def = PandocSettings Nothing [] False
