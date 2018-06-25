{-# LANGUAGE OverloadedStrings #-}

-- :set -XOverloadedStrings

-- |
-- This module provides Haskell representation of messages exchanged on the sakura.io platform.
-- 
-- https://sakura.io/docs/pages/spec/platform/message.html

module Network.SakuraIO.Platform.Message (
   ModuleID
  ,OutGoingMessage (..)
  ,ChannelID
  ,OGChannel (..)
  ,Latitude, Longitude, RangeM
  ,Coordinate (..)
  ,InComingMessage (..)
  ,ICChannel (..)
  ,ChannelValue (..)
  ) where


import Data.Time.Clock(UTCTime(..))
import Data.Int(Int32(..),Int64(..))
import Data.Word(Word8(..),Word32(..),Word64(..))
import qualified Data.ByteString as BS

import Control.Applicative(empty)
import Data.HashMap.Strict ((!))
import Data.Aeson.Types (Value(..))
import Data.Aeson.Types (FromJSON,parseJSON,(.:),Parser(..))
import Data.Aeson.Types (ToJSON,toJSON,object,(.=))

import Data.Monoid ((<>))

-- | Module ID starting with \'u\'.
type ModuleID = String

-- | Messages sent from sakura.io to external services.
--
-- It supports `FromJSON` to parse from json using @aeson@.
data OutGoingMessage = OGChannels ModuleID UTCTime [OGChannel]
                     | OGConnection ModuleID UTCTime Bool
                     | OGLocation ModuleID UTCTime (Maybe Coordinate)
                     | OGKeepalive UTCTime
                     deriving (Eq,Show)


-- | 7bit channel number.
type ChannelID = Word8

-- | Channel payload sent from sakura.io to external services as part of `OGChannels`.
--
-- It supports `FromJSON` to parse from json using @aeson@.
data OGChannel = OGChannel ChannelID UTCTime ChannelValue deriving (Eq,Show)

-- | Latitude.
type Latitude = Float
-- | Longitude.
type Longitude = Float
-- | Range in meters.
type RangeM = Int32

-- | Coordinate payload sent from sakura.io to external services as part of `OGLocation`.
--
-- It supports `FromJSON` to get json representation using @aeson@.
data Coordinate = Coordinate Latitude Longitude RangeM deriving (Eq,Show)


-- | Messages sent from external services to sakura.io.
--
-- It supports `ToJSON` to get json representation using @aeson@.
data InComingMessage = ICChannels ModuleID [ICChannel] deriving (Eq,Show)

-- | Channel payload sent from external services to sakura.io as part of `ICChannels`.
--
-- It supports `ToJSON` to get json representation using @aeson@.
data ICChannel = ICChannel ChannelID ChannelValue deriving (Eq,Show)


-- | Data exchanged between sakura.io and external services as part of `OGChannel` and `ICChannel`.
--
data ChannelValue = CVInt32 Int32
                  | CVWord32 Word32
                  | CVInt64 Int64
                  | CVWord64 Word64
                  | CVFloat Float
                  | CVDouble Double
                  | CVBytes BS.ByteString
                  deriving (Eq,Show)





instance FromJSON OutGoingMessage where
  parseJSON (Object v ) = do
    dt <- v .: "datetime"
    t  <- (v .: "type" :: Parser String)
    case t of
      "channels" -> case (v ! "payload") of
        Object pl -> do
          md <- v .: "module"
          cs <- pl .: "channels"
          return $ OGChannels md dt cs
        _ -> fail "playload must be Object"
      "connection" -> case (v ! "payload") of
        Object pl -> do
          md <- v .: "module"
          ol <- pl .: "is_online"
          return $ OGConnection md dt ol
        _ -> fail "playload must be Object"
      "location" -> case (v ! "payload") of
        Object pl -> do
          md <- v .: "module"
          cd <- pl .: "coordinate"
          return $ OGLocation md dt cd
        _ -> fail "playload must be Object"
      "keepalive"-> return $ OGKeepalive dt

  parseJSON _ = empty


instance FromJSON OGChannel where
  parseJSON (Object v) = do
    ch <- v .: "channel"
    dt <- v .: "datetime"
    t  <- v .: "type"
    v  <- case t of
      'i' -> CVInt32  <$> v .: "value"
      'I' -> CVWord32 <$> v .: "value"
      'l' -> CVInt64  <$> v .: "value"
      'L' -> CVWord64 <$> v .: "value"
      'f' -> CVFloat  <$> v .: "value"
      'd' -> CVDouble <$> v .: "value"
      'b' -> CVBytes . parseHex <$>  v .: "value"
    return $ OGChannel ch dt v

  parseJSON _ = empty



instance FromJSON Coordinate where
  parseJSON (Object v) =
    Coordinate <$> v .: "latitude" <*> v .: "longitude" <*> v .: "range_m"

  parseJSON _ = empty



instance ToJSON InComingMessage where
  toJSON (ICChannels md xs) =
    object ["type" .= ("channels"::String), "module" .= md,
            "payload" .=  object ["channels" .= xs]
           ]
    
instance ToJSON ICChannel where

  toJSON (ICChannel ch (CVInt32 i)) =
    object ["channel" .= ch, "type" .= ("i"::String),  "value" .= i]
    
  toJSON (ICChannel ch (CVWord32 i)) =
    object ["channel" .= ch, "type" .= ("I"::String),  "value" .= i]

  toJSON (ICChannel ch (CVInt64 l)) =
    object ["channel" .= ch, "type" .= ("l"::String),  "value" .= l]

  toJSON (ICChannel ch (CVWord64 l)) =
    object ["channel" .= ch, "type" .= ("L"::String),  "value" .= l]

  toJSON (ICChannel ch (CVFloat f)) =
    object ["channel" .= ch, "type" .= ("f"::String),  "value" .= f]

  toJSON (ICChannel ch (CVDouble d)) =
    object ["channel" .= ch, "type" .= ("d"::String),  "value" .= d]

  toJSON (ICChannel ch (CVBytes b)) =
    object ["channel" .= ch, "type" .= ("b"::String),  "value" .= toHex b]



parseHex :: [Char] -> BS.ByteString
parseHex = BS.pack . f . splitAt 2
  where
    f :: ([Char],[Char]) -> [Word8]
    f ([], _) =  []
    f (xs, ys) = (g xs) : (f $ splitAt 2 ys)

    g :: [Char] -> Word8
    g (h:l:[]) = (d h) * 16 + (d l)
      where
        d :: Char -> Word8
        d '0' = 0x0
        d '1' = 0x1
        d '2' = 0x2
        d '3' = 0x3
        d '4' = 0x4
        d '5' = 0x5
        d '6' = 0x6
        d '7' = 0x7
        d '8' = 0x8
        d '9' = 0x9
        d 'a' = 0xa
        d 'b' = 0xb
        d 'c' = 0xc
        d 'd' = 0xd
        d 'e' = 0xe
        d 'f' = 0xf
        d 'A' = 0xa
        d 'B' = 0xb
        d 'C' = 0xc
        d 'D' = 0xd
        d 'E' = 0xe
        d 'F' = 0xf


toHex :: BS.ByteString -> [Char]
toHex bs = foldr (\b->(<>)(g b)) [] $ BS.unpack bs
  where
    g :: Word8 -> [Char]
    g w = [d $ w `div` 16, d $ w `mod` 16]
      where
        d :: Word8 -> Char
        d 0x0 = '0'
        d 0x1 = '1'
        d 0x2 = '2'
        d 0x3 = '3'
        d 0x4 = '4'
        d 0x5 = '5'
        d 0x6 = '6'
        d 0x7 = '7'
        d 0x8 = '8'
        d 0x9 = '9'
        d 0xa = 'a'
        d 0xb = 'b'
        d 0xc = 'c'
        d 0xd = 'd'
        d 0xe = 'e'
        d 0xf = 'f'



