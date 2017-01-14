{-|
Module      : CommandLine
Description : Command-line parsers for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module CommandLine
    ( Options (..)
    , parseOptions
    ) where

import           Options.Applicative
import           Pansite

data Options = Options ServerConfig

portArg :: Parser Port
portArg = argument auto (metavar "PORT" <> help "port")

serverConfigParser :: Parser ServerConfig
serverConfigParser = ServerConfig <$> portArg

optionsParser :: Parser Options
optionsParser = Options <$> serverConfigParser

parseOptions :: IO Options
parseOptions = execParser optionsInfo
    where
        optionsInfo = info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "Run Pansite development server" <> header "Pansite development server")
