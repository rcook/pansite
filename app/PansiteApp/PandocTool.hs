{-|
Module      : PansiteApp.PandocTool
Description : Pandoc tool
Copyright   : (C) Richard Cook, 2017-2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PansiteApp.PandocTool (pandocTool) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Default
import           Data.List
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (writeFile)
import           Pansite
import           PansiteApp.Util
import           System.FilePath
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
                    ( Extension(..)
                    , HTMLMathMethod(..)
                    , Inline(..)
                    , WriterOptions(..)
                    , enableExtension
                    , readMarkdown
                    , readerExtensions
                    , runPure
                    , writeDocx
                    , writeHtml5
                    )
import           Text.Pandoc.Walk (walk)
import           Text.Pandoc.XML (toEntities)

data PandocSettings = PandocSettings
    { psNumberSections :: Bool
    , psVars :: [(String, String)]
    , psTemplatePath :: Maybe FilePath
    , psTableOfContents :: Bool
    , psReferenceDoc :: Maybe FilePath
    , psMathJaxEnabled :: Bool
    , psIncludeInHeaderPath :: Maybe FilePath
    , psIncludeBeforeBodyPath :: Maybe FilePath
    , psIncludeAfterBodyPath :: Maybe FilePath
    }

instance Default PandocSettings where
    def = PandocSettings False [] Nothing False Nothing False Nothing Nothing Nothing

updater :: PandocSettings -> UpdateContext -> Value -> Parser PandocSettings
updater PandocSettings{..} (UpdateContext resolveFilePath) =
    withObject "pandoc" $ \o ->
        let getFilePath key d = fmap (resolveFilePath <$>) (o .:? key .!= d)
        in PandocSettings
            <$> o .:? "number-sections" .!= psNumberSections
            <*> o .:? "vars" .!= psVars
            <*> getFilePath "template-path" psTemplatePath
            <*> o .:? "table-of-contents" .!= psTableOfContents
            <*> getFilePath "reference-doc" psReferenceDoc
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
        , writerReferenceDoc = psReferenceDoc
        , writerTemplate = mbTemplate
        , writerTableOfContents = psTableOfContents
        , writerHTMLMathMethod = htmlMathMethod
        , writerVariables = psVars3
        }

runner :: PandocSettings -> RunContext -> IO ()
runner
    ps@PandocSettings{..}
    (RunContext outputPath inputPaths _) = do

    putStrLn "PandocTool"
    putStrLn $ "  outputPath=" ++ outputPath
    putStrLn $ "  inputPaths=" ++ show inputPaths
    putStrLn $ "  psNumberSections=" ++ show psNumberSections
    putStrLn $ "  psReferenceDoc=" ++ show psReferenceDoc
    putStrLn $ "  psTableOfContents=" ++ show psTableOfContents
    putStrLn $ "  psTemplatePath=" ++ show psTemplatePath
    putStrLn $ "  psVars=" ++ show psVars

    md <- (intercalate "\n\n") <$> sequence (map readFileUtf8 inputPaths)

    let mdText = Text.pack md
        readerOpts = def
        exts = foldl'
                (flip enableExtension)
                (readerExtensions readerOpts)
                [Ext_backtick_code_blocks, Ext_yaml_metadata_block]
        readerOpts' = readerOpts { readerExtensions = exts }
        Right doc' = runPure $ readMarkdown readerOpts' mdText -- TODO: Irrefutable pattern
        doc = walk rewriteLinks doc'

    writerOpts <- mkWriterOptions ps

    -- TODO: Ugh. Let's make this less hacky. It works for now though.
    case (takeExtension outputPath) of
        ".docx" -> do
                        let Right docx = runPure $ writeDocx writerOpts doc -- TODO: Irrefutable pattern
                        BL.writeFile outputPath docx
        _ -> do
                        let Right html = runPure $ writeHtml5 writerOpts doc -- TODO: Irrefutable pattern
                            t = toEntities (Text.pack $ renderHtml html)
                        Text.writeFile outputPath t

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

pandocTool :: Tool
pandocTool = mkTool $ PandocSettings False [] Nothing False Nothing False Nothing Nothing Nothing

mkTool :: PandocSettings -> Tool
mkTool state = Tool "pandoc" updater' (runner state)
    where updater' ctx value = mkTool <$> updater state ctx value
