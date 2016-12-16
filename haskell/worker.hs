#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName       = "task_queue",
                               queueAutoDelete = False,
                               queueDurable    = True}

     qos ch 0 1 False

     BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
     consumeMsgs ch "task_queue" Ack deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ " [x] Received " <> body
  threadDelay (1000000 * n)
  BL.putStrLn " [x] Done"
  ackEnv metadata
  where
    body = msgBody msg
    n    = countDots body

countDots :: BL.ByteString -> Int
countDots = fromIntegral . BL.count '.'
