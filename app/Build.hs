module Build (build) where

import           Development.Shake
import           Development.Shake.FilePath
import           Pansite

build :: Config -> FilePath -> FilePath -> IO ()
build _ siteDir outputDir = shakeArgs shakeOptions { shakeFiles = outputDir } $ do
    outputDir <//> "*.html" %> \out -> do
        let md = siteDir </> (dropDirectory1 $ out -<.> "md")
        need [md]
        cmd "pandoc -o" [out] md
