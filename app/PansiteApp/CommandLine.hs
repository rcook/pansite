{-|
Module      : CommandLine
Description : Command-line parsers for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.CommandLine
    ( Options (..)
    , parseOptions
    ) where

import           Options.Applicative
import           Pansite

data Options = Options ServerConfig FilePath FilePath FilePath

portArg :: Parser Port
portArg = option auto
    (long "port"
    <> short 'p'
    <> value 3000
    <> metavar "PORT"
    <> help "port")

appDirParser :: Parser FilePath
appDirParser = strOption
    (long "app-dir"
    <> short 'a'
    <> value "_app"
    <> metavar "APPDIR"
    <> help "application directory")

outputDirParser :: Parser FilePath
outputDirParser = strOption
    (long "output-dir"
    <> short 'o'
    <> value "_output"
    <> metavar "OUTPUTDIR"
    <> help "output directory")

shakeDirParser :: Parser FilePath
shakeDirParser = strOption
    (long "shake-dir"
    <> short 's'
    <> value "_shake"
    <> metavar "SHAKEDIR"
    <> help "Shake directory")

serverConfigParser :: Parser ServerConfig
serverConfigParser = ServerConfig <$> portArg

optionsParser :: Parser Options
optionsParser = Options
    <$> serverConfigParser
    <*> appDirParser
    <*> outputDirParser
    <*> shakeDirParser

parseOptions :: IO Options
parseOptions = execParser optionsInfo
    where
        optionsInfo = info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "Run Pansite development server" <> header "Pansite development server")
