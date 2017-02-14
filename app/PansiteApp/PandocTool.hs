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
    , psReferenceDocx :: Maybe FilePath
    , psMathJaxEnabled :: Bool
    }

instance Default PandocSettings where
    def = PandocSettings False [] Nothing False Nothing False

updater :: ParserContext -> PandocSettings -> Value -> Parser PandocSettings
updater
    (ParserContext resolveFilePath)
    PandocSettings{..} =
    withObject "pandoc" $ \o -> do -- TODO: Should be able to use applicative style here!
        numberSections <- o .:? "number-sections" .!= psNumberSections

        vars <- o .:? "vars" .!= psVars

        mbTemplatePathTemp <- o .:? "template-path" .!= psTemplatePath
        let mbTemplatePath = resolveFilePath <$> mbTemplatePathTemp

        tableOfContents <- o .:? "table-of-contents" .!= psTableOfContents

        mbReferenceDocxTemp <- o .:? "reference-docx" .!= psReferenceDocx
        let mbReferenceDocx = resolveFilePath <$> mbReferenceDocxTemp

        mathJaxEnabled <- o .:? "mathjax" .!= psMathJaxEnabled

        return $ PandocSettings
                    numberSections
                    vars
                    mbTemplatePath
                    tableOfContents
                    mbReferenceDocx
                    mathJaxEnabled

mathJaxUrl :: String
mathJaxUrl = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML-full"

mkWriterOptions :: PandocSettings -> IO WriterOptions
mkWriterOptions PandocSettings{..} = do
    mbTemplate <- case psTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 templatePath

    let htmlMathMethod = if psMathJaxEnabled
                            then MathJax mathJaxUrl
                            else PlainMath

    return $ def
        { writerNumberSections = psNumberSections
        , writerReferenceDocx = psReferenceDocx
        , writerTemplate = mbTemplate
        , writerTableOfContents = psTableOfContents
        , writerHTMLMathMethod = htmlMathMethod
        , writerVariables = psVars
        }

runner :: ToolContext -> PandocSettings -> IO ()
runner
    (ToolContext outputPath inputPaths _)
    ps@PandocSettings{..} = do

    putStrLn "PandocTool"
    putStrLn $ "  outputPath=" ++ outputPath
    putStrLn $ "  inputPaths=" ++ show inputPaths
    putStrLn $ "  psNumberSections=" ++ show psNumberSections
    putStrLn $ "  psReferenceDocx=" ++ show psReferenceDocx
    putStrLn $ "  psTableOfContents=" ++ show psTableOfContents
    putStrLn $ "  psTemplatePath=" ++ show psTemplatePath
    putStrLn $ "  psVars=" ++ show psVars

    when -- TODO: Should support multiple inputs
        (length inputPaths /= 1)
        (error "Pandoc tool currently supports exactly one input")

    input <- readFileUtf8 (head inputPaths) -- TODO: Should support multiple inputs

    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
    writerOpts <- mkWriterOptions ps

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
