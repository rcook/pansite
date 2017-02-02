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
import           Data.Aeson.Types
import           Data.Default
import           Pansite

data CopySettings = CopySettings

instance Default CopySettings where
    def = CopySettings

updater :: ParserContext -> CopySettings -> Value -> Parser CopySettings
updater _ orig =
    withObject "copy" $ \o -> pure orig

runner :: ToolContext -> CopySettings -> IO ()
runner _ _ = error "Not implemented"

copyToolSpec :: ToolSpec
copyToolSpec = ToolSpec "copy" updater runner
