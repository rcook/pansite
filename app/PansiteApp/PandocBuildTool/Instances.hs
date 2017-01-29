{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocBuildTool.Instances () where

import           Control.Applicative
import           Data.Default
import           Data.Text (Text (..))
import           Data.Yaml
import           PansiteApp.PandocBuildTool.Types

instance FromJSON PandocSettings where
    parseJSON (Object v) = PandocSettings
        <$> v .: templatePathKey
    parseJSON _ = empty

instance ToJSON PandocSettings where
    toJSON (PandocSettings mbTemplatePath) = object
        [ templatePathKey .= mbTemplatePath
        ]

instance Default PandocSettings2 where
    def = PandocSettings2 Nothing []

templatePathKey :: Text
templatePathKey = "template-path"
