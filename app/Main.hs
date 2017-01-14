{-|
Module      : Main
Description : Main entrypoint for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           CommandLine
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Text
import           Pansite
import           Scan
import           System.IO

data State =
    AwaitingAAA |
    AwaitingBBB |
    AwaitingCCC |
    Success
    deriving (Eq, Show)

type StateVar = TVar State

newStateVar :: State -> IO StateVar
newStateVar = newTVarIO

checkState :: StateVar -> State -> State -> Bool -> ByteString -> STM RequestHandlerResult
checkState sVar expectedState nextState shouldShutDown responseText = do
    state <- readTVar sVar
    if state == expectedState
    then do
        writeTVar sVar nextState
        return $ if shouldShutDown then Done responseText else Valid responseText
    else return BadState

-- A simple test case expecting the sequence of requests [foo, bar, xyz]
-- Ignores requests to favicon.ico
requestHandler :: StateVar -> [Text] -> STM RequestHandlerResult
requestHandler sVar paths
    | paths == ["aaa"] = checkState sVar AwaitingAAA AwaitingBBB False "please respond with bbb"
    | paths == ["bbb"] = checkState sVar AwaitingBBB AwaitingCCC False "please respond with ccc"
    | paths == ["ccc"] = checkState sVar AwaitingCCC Success True "done"
    | paths == ["favicon.ico"] = return $ Valid "favicon.ico"
    | otherwise = return BadRequest

readChar :: IO Char
readChar = bracketHandle stdin $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- hGetChar stdin
    return c

loopUntilQ :: IO ()
loopUntilQ = do
    c <- readChar
    unless
        (c == 'q' || c == 'Q')
        loopUntilQ

run :: Options -> IO ()
run (Options config) = do
    sVar <- newStateVar AwaitingAAA
    {-
    runServer
        config
        (requestHandler sVar) $ do
            putStr "Q to quit: "
            hFlush stdout
            loopUntilQ
            putStrLn ""
            putStrLn "Done"
    -}
    startServer config (requestHandler sVar)

    putStr "Q to quit: "
    hFlush stdout
    loopUntilQ
    putStrLn ""
    putStrLn "Done"

    state <- readTVarIO sVar
    print state
    putStrLn $ if state == Success then "PASSED" else "FAILED"

--main :: IO ()
--main = parseOptions >>= run
main :: IO ()
main = doScan
