#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName       = "hello",
                               queueAutoDelete = False,
                               queueDurable    = False}

     putStrLn " [*] Waiting for messages. To exit press CTRL+C"
     consumeMsgs ch "hello" NoAck deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) =
  BL.putStrLn $ " [x] Received " <> msgBody msg
