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

module PansiteApp.CopyTool (copyTool) where

import           Data.Aeson
import           Pansite

data CopySettings = CopySettings

copyTool :: Tool
copyTool = mkTool CopySettings

mkTool :: CopySettings -> Tool
mkTool state = Tool "copy" updater (error "Not implemented")
    where updater _ value = mkTool <$> (withObject "copy" $ const (pure state)) value
