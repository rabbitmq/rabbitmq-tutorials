{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName    = "hello",
                               queuePassive = False,
                               queueDurable = False}

     publishMsg ch "" "hello"
                (newMsg {msgBody         = (BL.pack "Hello World!"),
                         msgDeliveryMode = Just NonPersistent})

     putStrLn " [x] Sent 'Hello World!'"
     closeConnection conn
