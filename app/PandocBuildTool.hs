module PandocBuildTool (pandocRender) where

import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc

readerOpts :: ReaderOptions
readerOpts = def

writerOpts :: WriterOptions
writerOpts = def

pandocRender :: String -> String
pandocRender s =
    let Right doc = readMarkdown readerOpts s -- TODO: Irrefutable pattern
    in renderHtml (writeHtml writerOpts doc)
