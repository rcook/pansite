{-# LANGUAGE OverloadedStrings #-}

module Pansite.AppConfig.Keys
    ( buildToolSettingsKey
    , firstNameKey
    , lastNameKey
    ) where

import           Data.Text (Text)

firstNameKey :: Text
firstNameKey = "first-name"

lastNameKey :: Text
lastNameKey = "last-name"

buildToolSettingsKey :: Text
buildToolSettingsKey = "build-tool-settings"
