#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName       = "hello",
                               queueAutoDelete = False,
                               queueDurable    = False}

     publishMsg ch "" "hello"
                (newMsg {msgBody         = "Hello World!",
                         msgDeliveryMode = Just NonPersistent})

     BL.putStrLn " [x] Sent 'Hello World!'"
     closeConnection conn
