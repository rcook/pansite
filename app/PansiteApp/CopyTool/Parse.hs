module PansiteApp.CopyTool.Parse (copySettingsParser) where

import           Data.Aeson (withObject)
import           Data.Yaml
import           PansiteApp.CopyTool.Types

copySettingsParser :: (FilePath -> FilePath) -> Value -> Parser CopySettings
copySettingsParser _ = withObject "copy" $ \_ -> pure CopySettings
