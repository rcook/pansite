module Build (build) where

import           Development.Shake
import           Development.Shake.FilePath

build :: FilePath -> FilePath -> IO ()
build siteDir outputDir = shakeArgs shakeOptions { shakeFiles = outputDir } $ do
    outputDir <//> "*.html" %> \out -> do
        let md = siteDir </> (dropDirectory1 $ out -<.> "md")
        need [md]
        cmd "pandoc -o" [out] md
