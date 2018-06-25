{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets  as WS
import Wuss (runSecureClient)
import Control.Concurrent (forkIO)
import Control.Monad (forever,unless)

import Data.Monoid ((<>))
import qualified Data.Aeson as AE

import Network.SakuraIO.Platform.Message


main :: IO ()
main = do
  ep:_ <- getArgs
  withSocketsDo $ runSecureClient "api.sakura.io" 443 ("/ws/v1/" <> ep) app


app :: WS.ClientApp ()
app conn = do
  
  _ <- forkIO $ forever $ do
    rdata <- WS.receiveData conn
    case AE.decode rdata of
      Nothing -> putStrLn $ "parse json failed ..."
      Just msg ->
        case msg of
          OGChannels md mdt (OGChannel ch cdt (CVInt32 i):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVInt32 "  <> (show i <> ")")
      
          OGChannels md mdt (OGChannel ch cdt (CVWord32 i):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVWord32 " <> (show i <> ")")

          OGChannels md mdt (OGChannel ch cdt (CVInt64 l):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVInt64 "  <> (show l <> ")")

          OGChannels md mdt (OGChannel ch cdt (CVWord64 l):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVWord64 " <> (show l <> ")")

          OGChannels md mdt (OGChannel ch cdt (CVFloat f):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVFloat "  <> (show f <> ")")

          OGChannels md mdt (OGChannel ch cdt (CVDouble d):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVDouble " <> (show d <> ")")
      
          OGChannels md mdt (OGChannel ch cdt (CVBytes b):_) ->
            putStrLn $ md <> ": " <> "OGChannel " <> (show ch) <> " " <> "(CVBytes "  <> (show b <> ")")

          OGConnection md mdt conn ->
            putStrLn $ md <> ": " <> "OGConnection " <> (show conn)
      
          OGLocation md mdt Nothing ->
            putStrLn $ md <> ": " <> "OGLocation " <> "Unknown"

          OGLocation md mdt (Just (Coordinate la lo rm)) ->
            putStrLn $ md <> ": " <> "OGLocation " <> (show la) <> " " <> (show lo) <> " " <> (show rm)

          OGKeepalive mdt ->
            putStrLn $ "OGKeepalive " <> (show mdt)
    

  let loop = do
        line <- getLine
        unless (null line) $ do
          let mid:ch:tname:value:_ = words line
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
              in WS.sendTextData conn (AE.encode msg)
            _ -> return ()
          loop
  loop

