{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as DT
import System.Environment (getArgs)
import Text.Printf

logsExchange = "direct_logs"

main :: IO ()
main = do
     args  <- getArgs
     let body     = bodyFor args
         severity = severityFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "direct",
                                     exchangeDurable = False}
     publishMsg ch logsExchange (DT.pack severity)
                (newMsg {msgBody = (BL.pack body),
                         msgDeliveryMode = Just NonPersistent})

     putStrLn $ printf " [x] Sent '%s'" (body)
     closeConnection conn


bodyFor :: [String] -> String
bodyFor [] = "Hello, world!"
bodyFor xs = unwords $ tail xs


severityFor :: [String] -> String
severityFor [] = "info"
severityFor xs = head xs
