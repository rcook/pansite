{-|
Module      : PansiteApp.PandocTool
Description : Pandoc tool
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.PandocTool (pandocToolSpec) where

import           Control.Monad
import           Data.Aeson
import           Data.Default
import           Data.Yaml
import           Pansite
import           PansiteApp.Util
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML

data PandocSettings = PandocSettings
    { psNumberSections :: Bool
    , psVars :: [(String, String)]
    , psTemplatePath :: Maybe FilePath
    }

instance Default PandocSettings where
    def = PandocSettings False [] Nothing

updater :: ParserContext -> PandocSettings -> Value -> Parser PandocSettings
updater
    (ParserContext resolveFilePath)
    (PandocSettings numberSectionsOrig varsOrig mbTemplatePathOrig) =
    withObject "pandoc" $ \o -> do
        numberSections <- o .:? "number-sections" .!= numberSectionsOrig
        vars <- o .:? "vars" .!= varsOrig
        mbTemplatePathTemp <- o .:? "template-path" .!= mbTemplatePathOrig
        let mbTemplatePath = resolveFilePath <$> mbTemplatePathTemp
        return $ PandocSettings numberSections vars mbTemplatePath

runner :: ToolContext -> PandocSettings -> IO ()
runner
    (ToolContext outputPath inputPaths _)
    (PandocSettings numberSections vars mbTemplatePath) = do

    putStrLn "PandocTool"
    putStrLn $ "  outputPath=" ++ outputPath
    putStrLn $ "  inputPaths=" ++ show inputPaths
    putStrLn $ "  numberSections=" ++ show numberSections
    putStrLn $ "  vars=" ++ show vars
    putStrLn $ "  mbTemplatePath=" ++ show mbTemplatePath

    when -- TODO: Should support multiple inputs
        (length inputPaths /= 1)
        (error "Pandoc tool currently supports exactly one input")

    input <- readFileUtf8 (head inputPaths) -- TODO: Should support multiple inputs

    mbTemplate <- case mbTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 templatePath

    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
        writerOpts = def
            { writerTemplate = mbTemplate
            , writerVariables = vars
            , writerNumberSections = numberSections
            }

    let html = toEntities (renderHtml (writeHtml writerOpts doc))
    writeFileUtf8 outputPath html

pandocToolSpec :: ToolSpec
pandocToolSpec = ToolSpec "pandoc" updater runner
