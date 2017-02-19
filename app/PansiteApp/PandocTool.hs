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

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Default
import           Data.List
import           Pansite
import           PansiteApp.Util
import           System.FilePath
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.XML

data PandocSettings = PandocSettings
    { psNumberSections :: Bool
    , psVars :: [(String, String)]
    , psTemplatePath :: Maybe FilePath
    , psTableOfContents :: Bool
    , psReferenceDocx :: Maybe FilePath
    , psMathJaxEnabled :: Bool
    , psIncludeInHeaderPath :: Maybe FilePath
    , psIncludeBeforeBodyPath :: Maybe FilePath
    , psIncludeAfterBodyPath :: Maybe FilePath
    }

instance Default PandocSettings where
    def = PandocSettings False [] Nothing False Nothing False Nothing Nothing Nothing

updater :: ParserContext -> PandocSettings -> Value -> Parser PandocSettings
updater (ParserContext resolveFilePath) PandocSettings{..} =
    withObject "pandoc" $ \o ->
        let getFilePath key def = fmap (resolveFilePath <$>) (o .:? key .!= def)
        in PandocSettings
            <$> o .:? "number-sections" .!= psNumberSections
            <*> o .:? "vars" .!= psVars
            <*> getFilePath "template-path" psTemplatePath
            <*> o .:? "table-of-contents" .!= psTableOfContents
            <*> getFilePath "reference-docx" psReferenceDocx
            <*> o .:? "mathjax" .!= psMathJaxEnabled
            <*> getFilePath "include-in-header-path" psIncludeInHeaderPath
            <*> getFilePath "include-before-body-path" psIncludeBeforeBodyPath
            <*> getFilePath "include-after-body-path" psIncludeAfterBodyPath

mathJaxUrl :: String
mathJaxUrl = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML-full"

maybeReadFileUtf8 :: Maybe FilePath -> IO (Maybe String)
maybeReadFileUtf8 (Just path) = Just <$> readFileUtf8 path
maybeReadFileUtf8 Nothing = return Nothing

mkWriterOptions :: PandocSettings -> IO WriterOptions
mkWriterOptions PandocSettings{..} = do
    mbTemplate <- maybeReadFileUtf8 psTemplatePath
    mbIncludeInHeader <- maybeReadFileUtf8 psIncludeInHeaderPath
    mbIncludeBeforeBody <- maybeReadFileUtf8 psIncludeBeforeBodyPath
    mbIncludeAfterBody <- maybeReadFileUtf8 psIncludeAfterBodyPath

    -- TODO: Let's do this more elegantly! Looks a little like a fold...
    let psVars1 = case mbIncludeInHeader of
                    Nothing -> psVars
                    Just s -> ("header-includes", s) : psVars
        psVars2 = case mbIncludeBeforeBody of
                    Nothing -> psVars1
                    Just s -> ("include-before", s) : psVars1
        psVars3 = case mbIncludeAfterBody of
                    Nothing -> psVars2
                    Just s -> ("include-after", s) : psVars2

    let htmlMathMethod = if psMathJaxEnabled
                            then MathJax mathJaxUrl
                            else PlainMath

    return $ def
        { writerNumberSections = psNumberSections
        , writerReferenceDocx = psReferenceDocx
        , writerTemplate = mbTemplate
        , writerTableOfContents = psTableOfContents
        , writerHTMLMathMethod = htmlMathMethod
        , writerVariables = psVars3
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

    md <- (intercalate "\n\n") <$> sequence (map readFileUtf8 inputPaths)

    let Right doc' = readMarkdown def md -- TODO: Irrefutable pattern
        doc = walk rewriteLinks doc'

    writerOpts <- mkWriterOptions ps

    -- TODO: Ugh. Let's make this less hacky. It works for now though.
    case (takeExtension outputPath) of
        ".docx" -> do
                        docx <- writeDocx writerOpts doc
                        BL.writeFile outputPath docx
        _ -> do
                        let html = toEntities (renderHtml (writeHtml writerOpts doc))
                        writeFileUtf8 outputPath html

-- TODO: This is very application-specific
-- TODO: Figure out how to allow this behaviour to be specified in app configuration
transformUrl :: String -> String
transformUrl url =
    let (path, ext) = splitExtension url
    in case ext of
        ".md" -> path
        _ -> url

-- TODO: This is very application-specific
-- TODO: Figure out how to allow this behaviour to be specified in app configuration
rewriteLinks :: Inline -> Inline
rewriteLinks (Link attr is (url, title)) = Link attr is (transformUrl url, title)
rewriteLinks i = i

pandocToolSpec :: ToolSpec
pandocToolSpec = ToolSpec "pandoc" updater runner
