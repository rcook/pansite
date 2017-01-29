module PansiteApp.CopyTool.Parser (copySettingsParser) where

import           Data.Aeson (withObject)
import           Data.Yaml
import           PansiteApp.CopyTool.Types

copySettingsParser :: Value -> Parser CopySettings
copySettingsParser = withObject "copy" $ \_ -> pure CopySettings
