{-|
Module      : Pansite.TestServer.Funcs
Description : Functions for Pansite test server
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Pansite.TestServer.Funcs
    ( runServer
    , startServer
    , waitServer
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy
import           Data.Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Pansite.Event
import           Pansite.TestServer.Types

-- |Run test server
-- Wraps execution of test server using given request handler
runServer :: ServerConfig -> RequestHandler -> IO () -> IO ()
runServer config handler action = bracket (startServer config handler) waitServer (const action)

-- |Start test server
-- Starts test server using specified request handler and returns
-- server information for use by stopServer etc.
startServer :: ServerConfig -> RequestHandler -> IO ServerInfo
startServer (ServerConfig port) handler = do
    -- Signalled when server is ready to handle connections
    ready <- newMEvent

    -- Number of active connections
    connectionCount <- newTVarIO (0 :: Int)

    -- Signalled when server has shut down
    shutdown <- newTMEventIO

    -- Application settings including event handlers
    let settings =
            setPort port $
            setBeforeMainLoop (setMEvent ready) $
            setOnOpen (const $ atomically $ modifyTVar' connectionCount (+1) >> return True) $
            setOnClose (const $ atomically $ modifyTVar' connectionCount (subtract 1)) $
            defaultSettings

    -- Application handler
    let app :: Application
        app req respond = do
            response <- atomically $ do
                let textResponse = return . responseLBS ok200 [(hContentType, "text/plain")]
                shouldRun <- isSetTMEvent shutdown
                if shouldRun
                then do
                    (shouldShutDown, responseText) <- callback (pathInfo req)
                    when shouldShutDown $ setTMEvent shutdown
                    textResponse responseText
                else return $ responseLBS serviceUnavailable503 [] ""
            respond response
            where
                callback :: [Text] -> STM (Bool, ByteString)
                callback paths = do
                    r <- handler paths
                    return $ case r of
                        Valid s -> (False, s)
                        Done s -> (True, s)
                        BadState -> (True, "BadState error")
                        BadRequest -> (True, "BadRequest error")

    -- Run server on separate thread and wait until it's ready
    void $ forkIO $ runSettings settings app
    waitMEvent ready
    return $ ServerInfo shutdown connectionCount

-- |Wait until test server has shut down
-- Waits until test server started using startServer function has shut down
waitServer :: ServerInfo -> IO ()
waitServer (ServerInfo shutdown connectionCount) = atomically $ do
    waitTMEvent shutdown
    value <- readTVar connectionCount
    when (value /= 0) retry
