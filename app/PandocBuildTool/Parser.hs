{-# LANGUAGE OverloadedStrings #-}

module PandocBuildTool.Parser
    ( pandocParser
    ) where

import           Data.Aeson (withObject)
import           Data.Text (Text (..))
import           PandocBuildTool.Types
import           PandocBuildTool.Instances
import           Data.Yaml

pandocParser :: Value -> Parser PandocSettings
pandocParser = withObject "pandoc" (.: pandocKey)

pandocKey :: Text
pandocKey = "pandoc"
