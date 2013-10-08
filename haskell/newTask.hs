{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import Text.Printf

main :: IO ()
main = do
     args  <- getArgs
     let body  = bodyFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareQueue ch newQueue {queueName       = "task_queue",
                               queueAutoDelete = False,
                               queueDurable    = True}

     publishMsg ch "" "task_queue"
                (newMsg {msgBody         = (BL.pack body),
                         msgDeliveryMode = Just Persistent})

     putStrLn $ printf " [x] Sent '%s'" (body)
     closeConnection conn

bodyFor :: [String] -> String
bodyFor [] = "Hello, world!"
bodyFor xs = unwords xs
