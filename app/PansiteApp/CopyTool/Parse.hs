module PansiteApp.CopyTool.Parse (copySettingsParser) where

import           Data.Aeson (withObject)
import           Data.Yaml
import           Pansite
import           PansiteApp.CopyTool.Types

copySettingsParser :: FilePathResolver -> Value -> Parser CopySettings
copySettingsParser _ = withObject "copy" $ \_ -> pure CopySettings
