{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import Text.Printf

logsExchange = "logs"

main :: IO ()
main = do
     args  <- getArgs
     let body = bodyFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareExchange ch newExchange {exchangeName = logsExchange, exchangeType = "fanout", exchangeDurable = False}
     publishMsg ch logsExchange ""
                (newMsg {msgBody = (BL.pack body),
                         msgDeliveryMode = Just NonPersistent})

     putStrLn $ printf " [x] Sent '%s'" (body)
     closeConnection conn


bodyFor :: [String] -> String
bodyFor [] = "Hello, world!"
bodyFor xs = unwords xs
