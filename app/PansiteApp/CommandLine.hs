{-|
Module      : PansiteApp.CommandLine
Description : Command-line parsers for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.CommandLine
    ( Command (..)
    , ServerConfig (..)
    , parseCommand
    ) where

import           Options.Applicative

-- TODO: Move into separate module
type Port = Int

-- TODO: Move into separate module
data ServerConfig = ServerConfig Port deriving Show

data Command = RunCommand ServerConfig FilePath FilePath | VersionCommand

portArg :: Parser Port
portArg = option auto
    (long "port"
    <> short 'p'
    <> value 3000
    <> metavar "PORT"
    <> help "port")

configParser :: Parser FilePath
configParser = strOption
    (long "config"
    <> short 'c'
    <> value ".pansite.yaml"
    <> metavar "CONFIG"
    <> help "path to YAML application configuration file")

outputDirParser :: Parser FilePath
outputDirParser = strOption
    (long "output-dir"
    <> short 'o'
    <> value "_output"
    <> metavar "OUTPUTDIR"
    <> help "output directory")

serverConfigParser :: Parser ServerConfig
serverConfigParser = ServerConfig <$> portArg

runCommandParser :: Parser Command
runCommandParser = RunCommand
    <$> serverConfigParser
    <*> configParser
    <*> outputDirParser

versionCommandParser :: Parser Command
versionCommandParser = flag' VersionCommand (short 'v' <> long "version" <> help "Show version")

commandParser :: Parser Command
commandParser = runCommandParser <|> versionCommandParser

parseCommand :: IO Command
parseCommand = execParser $ info
    (helper <*> commandParser)
    (fullDesc <> progDesc "Run Pansite development server" <> header "Pansite development server")
