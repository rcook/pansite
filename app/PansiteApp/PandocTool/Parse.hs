{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool.Parse
    ( pandocParser
    , pandocSettingsParser
    ) where

import           Data.Aeson (withObject)
import           Data.Text (Text (..))
import           Data.Yaml
import           PansiteApp.PandocTool.Types
import           PansiteApp.PandocTool.Instances

pandocParser :: Value -> Parser PandocSettings
pandocParser = withObject "pandoc" (.: pandocKey)

pandocKey :: Text
pandocKey = "pandoc"

templatePathKey :: Text
templatePathKey = "template-path"

varsKey :: Text
varsKey = "vars"

pandocSettingsParser :: Value -> Parser PandocSettings2
pandocSettingsParser = withObject "pandoc" $ \o -> PandocSettings2
    <$> o .:? templatePathKey
    <*> o .:? varsKey .!= []
