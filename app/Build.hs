module Build (build) where

import           Development.Shake
import           Development.Shake.FilePath
import           Pansite

build :: Config -> FilePath -> FilePath -> FilePath -> IO ()
build _ target siteDir outputDir = shake shakeOptions { shakeFiles = outputDir } $ do
    want [target]
    outputDir <//> "*.html" %> \out -> do
        let md = siteDir </> (dropDirectory1 $ out -<.> "md")
        need [md]
        cmd "pandoc -o" [out] md
