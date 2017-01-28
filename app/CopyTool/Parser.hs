module CopyTool.Parser (copySettingsParser) where

import           CopyTool.Types
import           Data.Aeson (withObject)
import           Data.Yaml

copySettingsParser :: Value -> Parser CopySettings
copySettingsParser = withObject "copy" $ \_ -> pure CopySettings
