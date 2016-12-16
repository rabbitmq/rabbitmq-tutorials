#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package safe
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import           Safe (atMay)
import           System.Environment (getArgs)

logsExchange = "topic_logs"

main :: IO ()
main = do
     args  <- getArgs
     let body     = bodyFor args
         severity = severityFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "topic",
                                     exchangeDurable = False}
     publishMsg ch logsExchange severity
                (newMsg {msgBody = body,
                         msgDeliveryMode = Just NonPersistent})

     BL.putStrLn $ " [x] Sent " <> body
     closeConnection conn

bodyFor :: [String] -> BL.ByteString
bodyFor xs = maybe "Hello world" BL.pack (atMay xs 1)

severityFor :: [String] -> DT.Text
severityFor xs = maybe "anonymous.info" DT.pack (atMay xs 0)
