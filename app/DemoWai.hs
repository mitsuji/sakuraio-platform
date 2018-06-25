{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as H

import Data.Monoid ((<>))
import qualified Data.Aeson as AE

import Network.SakuraIO.Platform.Message


main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ demoApp

demoApp :: Wai.Application
demoApp req respond = do
  body <- Wai.strictRequestBody req
  case AE.decode body of
    Nothing -> do
      putStrLn $ "parse json failed ..."
      respond $ Wai.responseLBS H.status500 [("Content-Type","text/plain")] "NG"
      
    Just msg -> do
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
    
      respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] "OK"
      

