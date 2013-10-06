{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
     conn <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch   <- openChannel conn

     declareQueue ch newQueue {queueName       = "task_queue",
                               queueAutoDelete = False,
                               queueDurable    = True}

     publishMsg ch "" "task_queue"
                (newMsg {msgBody         = (BL.pack "a.b.c.d.e"),
                         msgDeliveryMode = Just Persistent})

     putStrLn " [x] Sent 'a.b.c.d.e'"
     closeConnection conn
