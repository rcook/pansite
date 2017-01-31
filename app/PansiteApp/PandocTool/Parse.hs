{-|
Module      : PansiteApp.PandocTool.Parse
Description : Pandoc parser
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool.Parse (pandocSettingsParser) where

import           Data.Aeson (withObject)
import           Data.Text (Text)
import           Data.Yaml
import           Pansite
import           PansiteApp.PandocTool.Types

numberSectionsKey :: Text
numberSectionsKey = "number-sections"

templatePathKey :: Text
templatePathKey = "template-path"

varsKey :: Text
varsKey = "vars"

pandocSettingsParser :: FilePathResolver -> Value -> Parser PandocSettings
pandocSettingsParser resolveFilePath = withObject "pandoc" $ \o -> do
    mbTemplatePathTemp <- o .:? templatePathKey
    let mbTemplatePath = resolveFilePath <$> mbTemplatePathTemp
    vars <- o .:? varsKey .!= []
    numberSections <- o .:? numberSectionsKey .!= False
    return $ PandocSettings mbTemplatePath vars numberSections
