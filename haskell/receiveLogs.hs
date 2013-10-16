{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Concurrent (threadDelay)

logsExchange = "logs"
       
main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "fanout",
                                     exchangeDurable = False}
     (q, _, _) <- declareQueue ch newQueue {queueName       = "",
                                            queueAutoDelete = True,
                                            queueDurable    = False}
     bindQueue ch q logsExchange ""

     putStrLn " [*] Waiting for messages. to Exit press CTRL+C"
     consumeMsgs ch q Ack deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  putStrLn $ " [x] Received " ++ body
  threadDelay (1000 * n)
  putStrLn $ " [x] Done"
  ackEnv metadata
  where
    body = (BL.unpack $ msgBody msg)
    n    = countDots body



countDots :: [Char] -> Int
countDots s = length $ filter (\c -> c == '.') s
