{-# LANGUAGE OverloadedStrings #-}

module ToolSettings (demoToolSettings) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import qualified Data.HashMap.Strict as HashMap
import           Data.Yaml
import           Pansite

type PandocVariable = (String, String)

data PandocSettings = PandocSettings
    (Maybe FilePath)
    [PandocVariable]
    deriving Show

data CopySettings = CopySettings deriving Show

instance Default PandocSettings where
    def = PandocSettings Nothing []

instance Default CopySettings where
    def = CopySettings

pandocParser :: Value -> Parser PandocSettings
pandocParser = withObject "pandoc" $ \o -> PandocSettings
    <$> o .:? "template-path"
    <*> o .:? "vars" .!= []

runPandoc :: PandocSettings -> ToolRunner
runPandoc (PandocSettings mbTemplatePath mbVars) input =
    "[pandoc: " ++ input ++ ", template-path=" ++ show mbTemplatePath ++ ", vars=" ++ show mbVars ++ "]"

copyParser :: Value -> Parser CopySettings
copyParser = withObject "copy" $ \_ -> pure CopySettings

runCopy :: CopySettings -> ToolRunner
runCopy _ input = "[copy: " ++ input ++ "]"

demoToolSettings :: IO ()
demoToolSettings = do
    let tools =
            [ Tool "pandoc" pandocParser runPandoc
            , Tool "copy" copyParser runCopy
            ]
        yaml =  "first-name: Richard\n\
                \last-name: Cook\n\
                \referrers:\n\
                \  website1.com:\n\
                \    page1: 3\n\
                \    page2: 4\n\
                \  website2.com:\n\
                \    page: 10\n\
                \build-tool-settings:\n\
                \  pandoc:\n\
                \    template-path: /path/to/template\n\
                \    vars:\n\
                \    - [css, b]\n\
                \    - [css, d]\n\
                \    k1: v1\n\
                \  copy:\n\
                \    x: y\n"
    case (decodeEither' yaml :: Either ParseException Value) of
        Left ex -> putStrLn $ "Parse exception: " ++ show ex
        Right value -> do
            case parse (appConfigParser tools) value of
                Error message -> putStrLn $ "Error: " ++ message
                Success (AppConfig2 firstName lastName runnerMap) -> do
                    putStrLn $ "firstName: " ++ firstName
                    putStrLn $ "lastName: " ++ lastName
                    putStrLn $ "runners: " ++ show (HashMap.keys runnerMap)
                    let result = case HashMap.lookup "pandoc" runnerMap of
                                    Nothing -> "no such build tool"
                                    Just r -> r "input"
                    putStrLn $ "result: " ++ result
