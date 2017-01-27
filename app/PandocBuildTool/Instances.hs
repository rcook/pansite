{-# LANGUAGE OverloadedStrings #-}

module PandocBuildTool.Instances () where

import           Control.Applicative
import           Data.Text (Text (..))
import           Data.Yaml
import           PandocBuildTool.Types

instance FromJSON PandocSettings where
    parseJSON (Object v) = PandocSettings
        <$> v .: templatePathKey
    parseJSON _ = empty

instance ToJSON PandocSettings where
    toJSON (PandocSettings mbTemplatePath) = object
        [ templatePathKey .= mbTemplatePath
        ]

templatePathKey :: Text
templatePathKey = "template-path"
