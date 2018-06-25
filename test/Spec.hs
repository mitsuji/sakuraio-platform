{-# LANGUAGE OverloadedStrings #-}

-- :set -XOverloadedStrings

import Test.Hspec
-- import Control.Exception (evaluate)

import Network.SakuraIO.Platform.Message 
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime(..),DiffTime(..))
import Data.Time.Calendar (fromGregorian)


main :: IO ()
main = hspec $ do
  describe "Network.SakuraIO.Platform" $ do
    describe "Message" $ do

      json <- runIO $ LBS.readFile "test/data/OGChannels_Word32_0.json.txt"
      it "32 bit unsigned integer 0" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVWord32 0)]))

      json <- runIO $ LBS.readFile "test/data/OGChannels_Word32_12345.json.txt"
      it "32 bit unsigned integer 12345" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVWord32 12345)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Int32_0.json.txt"
      it "32 bit signed integer 0" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVInt32 0)]))

      json <- runIO $ LBS.readFile "test/data/OGChannels_Int32_12345.json.txt"
      it "32 bit signed integer 12345" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVInt32 12345)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Word64_0.json.txt"
      it "64 bit unsigned integer 0" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVWord64 0)]))

      json <- runIO $ LBS.readFile "test/data/OGChannels_Word64_12345.json.txt"
      it "64 bit unsigned integer 12345" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVWord64 12345)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Int64_0.json.txt"
      it "64 bit signed integer 0" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVInt64 0)]))

      json <- runIO $ LBS.readFile "test/data/OGChannels_Int64_12345.json.txt"
      it "64 bit signed integer 12345" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVInt64 12345)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Float_0.001.json.txt"
      it "float 0.001" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVFloat 0.001)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Double_0.001.json.txt"
      it "double 0.001" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVDouble 0.001)]))
        
      json <- runIO $ LBS.readFile "test/data/OGChannels_Bytes.json.txt"
      it "bytes abcdefgh ijklmnop ABCEDFGH IJKLMNOP" $ do
        AE.decode json `shouldBe`
          (Just
           (OGChannels "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            [OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVBytes "abcdefgh")
            ,OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVBytes "ijklmnop")
            ,OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVBytes "ABCDEFGH")
            ,OGChannel 0
             (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770023483))
             (CVBytes "IJKLMNOP")
            ]))


      json <- runIO $ LBS.readFile "test/data/OGConnection_true.json.txt"
      it "connection true" $ do
        AE.decode json `shouldBe`
          (Just
           (OGConnection "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            True))
          
      json <- runIO $ LBS.readFile "test/data/OGConnection_false.json.txt"
      it "connection false" $ do
        AE.decode json `shouldBe`
          (Just
           (OGConnection "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            False))

      json <- runIO $ LBS.readFile "test/data/OGLocation_unknown.json.txt"
      it "location unknown" $ do
        AE.decode json `shouldBe`
          (Just
           (OGLocation "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            Nothing))

      json <- runIO $ LBS.readFile "test/data/OGLocation_0.json.txt"
      it "location 0" $ do
        AE.decode json `shouldBe`
          (Just
           (OGLocation "uXXXXXXXXXXX"
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
            (Just (Coordinate 0.0 0.0 0))))

      json <- runIO $ LBS.readFile "test/data/OGKeepalive.json.txt"
      it "keepalive" $ do
        AE.decode json `shouldBe`
          (Just
           (OGKeepalive
            (UTCTime (fromGregorian 2018 06 21) (fromClock 1 10 57.770020629))
           )
          )





fromClock :: Int -> Int -> Double -> DiffTime
fromClock h m s =
  fromRational $ (fromIntegral h) * 60 * 60 + (fromIntegral m) * 60 + (toRational s)
