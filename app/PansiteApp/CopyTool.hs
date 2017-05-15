{-|
Module      : PansiteApp.CopyTool
Description : Copy tool
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.CopyTool (copyToolSpec) where

import           Data.Aeson
import           Pansite

data CopySettings = CopySettings

copyToolSpec :: ToolConfig
copyToolSpec = mkToolConfig CopySettings

mkToolConfig :: CopySettings -> ToolConfig
mkToolConfig state = ToolConfig "copy" updater runner
    where
        updater _ value = mkToolConfig <$> (withObject "copy" $ const (pure state)) value
        runner _ = error "Not implemented"
