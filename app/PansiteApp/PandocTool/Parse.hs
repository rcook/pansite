{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool.Parse (pandocSettingsParser) where

import           Data.Aeson (withObject)
import           Data.Text (Text)
import           Data.Yaml
import           PansiteApp.PandocTool.Types

templatePathKey :: Text
templatePathKey = "template-path"

varsKey :: Text
varsKey = "vars"

pandocSettingsParser :: (FilePath -> FilePath) -> Value -> Parser PandocSettings
pandocSettingsParser makeTargetPath = withObject "pandoc" $ \o -> do
    mbTemplatePathTemp <- o .:? templatePathKey
    let mbTemplatePath = makeTargetPath <$> mbTemplatePathTemp
    vars <- o .:? varsKey .!= []
    return $ PandocSettings mbTemplatePath vars
