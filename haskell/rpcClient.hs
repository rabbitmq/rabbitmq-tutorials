#!/usr/bin/env stack
-- stack --install-ghc runghc --package bytestring --package text --package amqp --package uuid
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent         (MVar, newEmptyMVar, putMVar,
                                             takeMVar)
import           Control.Monad              (when)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Network.AMQP

type QueueName = Text

main :: IO ()
main = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    ch   <- openChannel conn

    putStrLn " [x] Requesting fib(30)"
    res <- callFib ch rpcQueue 30
    putStrLn $ " [.] Got '" ++ show res ++ "'"

    closeConnection conn
  where
    rpcQueue = "rpc_queue"

callFib :: Channel -> QueueName -> Int -> IO Int
callFib ch queue n = do
    cid <- genCorrelationId
    rqn <- declareReplyQueue

    let body = BL.pack . show $ n
    let message = newMsg {msgCorrelationID = Just cid, msgReplyTo = Just rqn, msgBody = body}
    publishMsg ch "" queue message

    m <- newEmptyMVar
    consumeMsgs ch rqn Ack $ handleResponse cid m

    res <- takeMVar m
    return res
  where
    genCorrelationId = toText <$> nextRandom
    declareReplyQueue = do
        let opts = newQueue {queueAutoDelete = True, queueExclusive = True}
        (rqn, _, _) <- declareQueue ch opts
        return rqn

handleResponse :: Text -> MVar Int -> (Message, Envelope) -> IO ()
handleResponse corrId m (msg, envelope) = do
    let msgCorrId = fromJust (msgCorrelationID msg)
    when (msgCorrId == corrId) $ do
        res <- readIO (BL.unpack . msgBody $ msg)
        putMVar m res
        ackEnv envelope
