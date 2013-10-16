{-# OPTIONS -XOverloadedStrings #-}

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as DT
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Monad (forM)

logsExchange = "direct_logs"

main :: IO ()
main = do
     conn       <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch         <- openChannel conn
     severities <- getArgs

     declareExchange ch newExchange {exchangeName    = logsExchange,
                                     exchangeType    = "direct",
                                     exchangeDurable = False}
     (q, _, _) <- declareQueue ch newQueue {queueName       = "",
                                            queueAutoDelete = True,
                                            queueDurable    = False}
     forM severities (\s -> bindQueue ch q logsExchange (DT.pack s))

     putStrLn " [*] Waiting for messages. to Exit press CTRL+C"
     consumeMsgs ch q Ack deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  putStrLn $ printf " [x] %s:%s" (DT.unpack $ envRoutingKey metadata) body
  putStrLn $ " [x] Done"
  ackEnv metadata
  where
    body = (BL.unpack $ msgBody msg)
