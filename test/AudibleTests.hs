{-# LANGUAGE OverloadedStrings #-}
module AudibleTests
  ( audibleTests
  ) where

-- base
import Control.Monad.IO.Class
import Data.IORef

-- hosc
import Sound.OSC

-- hspec
import Test.Hspec

-- Internal
import Sound.SC3.Jiffy
import Sound.SC3.Jiffy.Encode


-- ------------------------------------------------------------------------
--
-- Logging packets
--
-- ------------------------------------------------------------------------

newtype OSCLog a = OSCLog {unOSCLog :: IORef [Packet] -> IO a}

runOSCLog :: OSCLog a -> IO [Packet]
runOSCLog (OSCLog m) = do
  ref <- newIORef []
  _ <- m ref
  reverse <$> readIORef ref

instance Functor OSCLog where
  fmap f (OSCLog m) = OSCLog (\r -> fmap f (m r))

instance Applicative OSCLog where
  pure x = OSCLog (\_ -> pure x)
  OSCLog f <*> OSCLog x = OSCLog (\r -> f r <*> x r)

instance Monad OSCLog where
  return = pure
  OSCLog m >>= k = OSCLog (\r -> m r >>= \a -> unOSCLog (k a) r)

instance MonadIO OSCLog where
  liftIO io = OSCLog (\_ -> io)

instance SendOSC OSCLog where
  sendPacket p = OSCLog (\r -> modifyIORef' r (p:))

instance RecvOSC OSCLog where
  recvPacket = error "OSCLog: recvPacket"

instance DuplexOSC OSCLog

instance Transport OSCLog


-- ------------------------------------------------------------------------
--
-- Test codes
--
-- ------------------------------------------------------------------------

ug01 :: UGen
ug01 = out 0 (sinOsc AR 440 0)

packet01 :: IO [Packet]
packet01 = runOSCLog (play ug01)

sd01 :: Synthdef
sd01 = synthdef "sd01" ug01

packet02 :: IO [Packet]
packet02 = runOSCLog (play sd01)

audibleTests :: Spec
audibleTests = do
  describe "play_ugen" $ do
    packets <- runIO packet01
    let expected =
          [Packet_Message (Message "/d_recv" [Blob graph, Blob sn])]
        graph = encode_graphdef (ugen_to_graphdef "anon" ug01)
        sn = encodeMessage (s_new "anon" (-1) AddToHead 1 [])
    it "sends_d_recv_and_s_new" $ packets `shouldBe` expected

  describe "play_synthdef" $ do
    packets <- runIO packet02
    let expected =
          [Packet_Message (Message "/d_recv" [Blob g, Blob sn])]
        g = synthdefData sd01
        sn = encodeMessage (s_new "sd01" (-1) AddToHead 1 [])
    it "sends_d_recv_and_s_new" $ packets `shouldBe` expected
