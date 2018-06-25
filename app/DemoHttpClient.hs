{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid ((<>))
import qualified Data.Aeson as AE

import Network.SakuraIO.Platform.Message


main :: IO ()
main = do
  httpManager <- HC.newManager tlsManagerSettings
  ep:mid:ch:tname:value:_ <- getArgs
  
  case tname of
    xs | elem xs ["i","I","l","L","f","d","b"] ->
      let msg = case tname of
            "i" -> ICChannels mid [ICChannel (read ch) (CVInt32  (read value))]
            "I" -> ICChannels mid [ICChannel (read ch) (CVWord32 (read value))]
            "l" -> ICChannels mid [ICChannel (read ch) (CVInt64  (read value))]
            "L" -> ICChannels mid [ICChannel (read ch) (CVWord64 (read value))]
            "f" -> ICChannels mid [ICChannel (read ch) (CVFloat  (read value))]
            "d" -> ICChannels mid [ICChannel (read ch) (CVDouble (read value))]
            "b" -> ICChannels mid [ICChannel (read ch) (CVBytes  (read value))] -- [TODO]
      in LBS.putStr =<< sendMessage httpManager ep msg
    
    _ -> do
      putStrLn "sakuraio-platform-demo-http-client"
      putStrLn ""
      putStrLn "Usage: sakuraio-platform-demo-http-client-exe [endpoint] [module] [channel] [type] [value]"
      putStrLn ""
      putStrLn "Avaliable type:"
      putStrLn "\ti: signed 32 bit integer"
      putStrLn "\tI: unsigned 32 bit integer"
      putStrLn "\tl: signed 64 bit integer"
      putStrLn "\tL: unsigned 64 bit integer"
      putStrLn "\tf: float"
      putStrLn "\td: double"
      putStrLn "\tb: byte array"


sendMessage :: HC.Manager -> String -> InComingMessage -> IO LBS.ByteString
sendMessage man ep msg = do
  req <- HC.parseRequest $ "https://api.sakura.io/incoming/v1/" <> ep
  let req' = req {HC.method = "POST", HC.requestBody = HC.RequestBodyLBS $ AE.encode msg}
  res <- HC.httpLbs req' man
  return $ HC.responseBody res


-- for repl
sendMessage' :: String -> InComingMessage -> IO LBS.ByteString
sendMessage' ep msg =
  HC.newManager tlsManagerSettings >>= \man -> sendMessage man ep msg

