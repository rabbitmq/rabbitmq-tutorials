{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName       = "hello",
                               queueAutoDelete = False,
                               queueDurable    = False}

     putStrLn " [*] Waiting for messages. to Exit press CTRL+C"
     consumeMsgs ch "hello" NoAck deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  putStrLn $ " [x] Received " ++ (BL.unpack $ msgBody msg)
