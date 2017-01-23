{-# LANGUAGE OverloadedStrings #-}

module Pansite.Target (foo) where

import qualified Data.ByteString.Char8 as C8
import           Data.Text (Text (..))
import Data.Yaml

data BuildTool = Pandoc

data Target = Target FilePath BuildTool [FilePath]

data Container = Container [Target]

{-
instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "key"
        <*> v .: inputsKey
    parseJSON _ = empty
-}

buildToolKey :: Text
buildToolKey = "build-tool"

dependenciesKey :: Text
dependenciesKey = "dependencies"

instance ToJSON BuildTool where
    toJSON Pandoc = "pandoc"

instance ToJSON Target where
    toJSON (Target path buildTool dependencies) = object
        [ "path" .= path
        , buildToolKey .= buildTool
        , dependenciesKey .= dependencies
        ]

instance ToJSON Container where
    toJSON (Container targets) = object
        [ "targets" .= targets
        ]


foo :: IO ()
foo = do
    C8.putStrLn $ encode (Container [Target "foo.html" Pandoc ["foo.md", "bar.md"], Target "bar.html" Pandoc ["foo.md", "bar.md"]])
