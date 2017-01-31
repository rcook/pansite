{-|
Module      : PansiteApp.CopyTool.Parse
Description : Copy parser
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.CopyTool.Parse (copySettingsParser) where

import           Data.Aeson (withObject)
import           Data.Yaml
import           Pansite
import           PansiteApp.CopyTool.Types

copySettingsParser :: FilePathResolver -> Value -> Parser CopySettings
copySettingsParser _ = withObject "copy" $ \_ -> pure CopySettings
