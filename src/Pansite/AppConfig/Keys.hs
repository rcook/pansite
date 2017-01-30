{-# LANGUAGE OverloadedStrings #-}

module Pansite.AppConfig.Keys
    ( buildToolSettingsKey
    ) where

import           Data.Text (Text)

buildToolSettingsKey :: Text
buildToolSettingsKey = "build-tool-settings"
