#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import           System.Environment (getArgs)

main :: IO ()
main = do
     args  <- getArgs
     let body = bodyFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     publishMsg ch "" "task_queue"
                (newMsg {msgBody         = body,
                         msgDeliveryMode = Just Persistent})

     BL.putStrLn $ " [x] Sent " <> body
     closeConnection conn

bodyFor :: [String] -> BL.ByteString
bodyFor [] = "Hello, world!"
bodyFor xs = BL.pack . unwords $ xs
