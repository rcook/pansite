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
{-# LANGUAGE RecordWildCards #-}

module PansiteApp.PandocTool (pandocToolSpec) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Default
import           Pansite
import           PansiteApp.Util
import           System.FilePath
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML

data PandocSettings = PandocSettings
    { psNumberSections :: Bool
    , psVars :: [(String, String)]
    , psTemplatePath :: Maybe FilePath
    , psTableOfContents :: Bool
    }

instance Default PandocSettings where
    def = PandocSettings False [] Nothing False

updater :: ParserContext -> PandocSettings -> Value -> Parser PandocSettings
updater
    (ParserContext resolveFilePath)
    (PandocSettings numberSectionsOrig varsOrig mbTemplatePathOrig tableOfContentsOrig) =
    withObject "pandoc" $ \o -> do -- TODO: Should be able to use applicative style here!
        numberSections <- o .:? "number-sections" .!= numberSectionsOrig
        vars <- o .:? "vars" .!= varsOrig
        mbTemplatePathTemp <- o .:? "template-path" .!= mbTemplatePathOrig
        let mbTemplatePath = resolveFilePath <$> mbTemplatePathTemp
        tableOfContents <- o .:? "table-of-contents" .!= tableOfContentsOrig
        return $ PandocSettings numberSections vars mbTemplatePath tableOfContents

runner :: ToolContext -> PandocSettings -> IO ()
runner
    (ToolContext outputPath inputPaths _)
    PandocSettings{..} = do

    putStrLn "PandocTool"
    putStrLn $ "  outputPath=" ++ outputPath
    putStrLn $ "  inputPaths=" ++ show inputPaths
    putStrLn $ "  psNumberSections=" ++ show psNumberSections
    putStrLn $ "  psTableOfContents=" ++ show psTableOfContents
    putStrLn $ "  psTemplatePath=" ++ show psTemplatePath
    putStrLn $ "  psVars=" ++ show psVars

    when -- TODO: Should support multiple inputs
        (length inputPaths /= 1)
        (error "Pandoc tool currently supports exactly one input")

    input <- readFileUtf8 (head inputPaths) -- TODO: Should support multiple inputs

    mbTemplate <- case psTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 templatePath

    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
        writerOpts = def
            { writerNumberSections = psNumberSections
            , writerTemplate = mbTemplate
            , writerTableOfContents = psTableOfContents
            , writerVariables = psVars
            }

    -- TODO: Ugh. Let's make this less hacky. It works for now though.
    case (takeExtension outputPath) of
        ".docx" -> do
                        docx <- writeDocx writerOpts doc
                        BL.writeFile outputPath docx
        _ -> do
                        let html = toEntities (renderHtml (writeHtml writerOpts doc))
                        writeFileUtf8 outputPath html

pandocToolSpec :: ToolSpec
pandocToolSpec = ToolSpec "pandoc" updater runner
