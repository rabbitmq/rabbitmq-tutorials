#!/usr/bin/env stack
-- stack --install-ghc runghc --package bytestring --package text --package amqp
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent         (MVar, newEmptyMVar, putMVar,
                                             takeMVar)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromJust)
import           Network.AMQP

main :: IO ()
main = do
    conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
    ch    <- openChannel conn

    qos ch 0 1 False
    declareQueue ch newQueue {queueName = rpcQueue}

    m <- newEmptyMVar
    consumeMsgs ch rpcQueue Ack $ handleRequest ch m
    putStrLn " [x] Awaiting RPC requests"
    takeMVar m

    closeConnection conn
  where
    rpcQueue = "rpc_queue"

handleRequest :: Channel -> MVar () -> (Message, Envelope) -> IO ()
handleRequest ch m (msg, envelope) = do
    n <- readIO . BL.unpack . msgBody $ msg
    putStrLn $ " [.] fib(" ++ show n ++ ")"

    let result = fib n
    let response = newMsg { msgCorrelationID = msgCorrelationID msg
                          , msgBody = BL.pack . show $ result
                          }
    publishMsg ch "" replyTo response
    ackEnv envelope
    putMVar m ()
  where
    replyTo = fromJust $ msgReplyTo msg

fib :: Int -> Int
fib n
    | n >= 2    = fib (n - 1) + fib (n - 2)
    | n == 1    = 1
    | otherwise = 0
