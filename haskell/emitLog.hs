#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid ((<>))
import System.Environment (getArgs)

logsExchange = "logs"

main :: IO ()
main = do
     args  <- getArgs
     let body = bodyFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "fanout",
                                     exchangeDurable = False}
     publishMsg ch logsExchange ""
                (newMsg {msgBody = body,
                         msgDeliveryMode = Just NonPersistent})

     BL.putStrLn $ " [x] Sent " <> body
     closeConnection conn

bodyFor :: [String] -> BL.ByteString
bodyFor [] = "Hello, world!"
bodyFor xs = BL.pack . unwords $ xs
