{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool.Parse (pandocSettingsParser) where

import           Data.Aeson (withObject)
import           Data.Text (Text (..))
import           Data.Yaml
import           PansiteApp.PandocTool.Types

templatePathKey :: Text
templatePathKey = "template-path"

varsKey :: Text
varsKey = "vars"

pandocSettingsParser :: Value -> Parser PandocSettings
pandocSettingsParser = withObject "pandoc" $ \o -> PandocSettings
    <$> o .:? templatePathKey
    <*> o .:? varsKey .!= []
