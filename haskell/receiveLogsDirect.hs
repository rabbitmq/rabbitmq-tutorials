#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT
import           System.Environment (getArgs)

logsExchange = "direct_logs"

main :: IO ()
main = do
     conn       <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch         <- openChannel conn
     severities <- getArgs

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "direct",
                                     exchangeDurable = False}
     (q, _, _) <- declareQueue ch newQueue {queueName       = "",
                                            queueAutoDelete = True,
                                            queueDurable    = False}
     forM_ severities (bindQueue ch q logsExchange . DT.pack)

     BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
     consumeMsgs ch q Ack deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ " [x] " <> key <> ":" <> body
  BL.putStrLn " [x] Done"
  ackEnv metadata
  where
    body = msgBody msg
    key  = BL.fromStrict . DT.encodeUtf8 $ envRoutingKey metadata
