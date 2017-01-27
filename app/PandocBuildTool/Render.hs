module PandocBuildTool.Render
    ( pandocRender
    ) where

import           Build
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           Paths_pansite

-- TODO: Eventually extract this as a configuration setting in app.yaml
cssUrls :: [FilePath]
cssUrls = ["css/buttondown.css"]

pandocRender :: RenderOpts -> String -> String
pandocRender (RenderOpts template) s =
    let Right doc = readMarkdown def s -- TODO: Irrefutable pattern
        -- TODO: Most, if not all, of these settings should be pulled from
        -- app.yaml etc.
        writerOpts = def
            { writerTemplate = Just template
            , writerVariables = map (\x -> ("css", x)) cssUrls
            , writerNumberSections = True
            }
    in toEntities (renderHtml (writeHtml writerOpts doc))
