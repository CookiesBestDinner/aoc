{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day23 where

import           Common
import           Control.Concurrent           (yield)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Data.Vector                  as V
import           Intcode
import           Protolude


main :: Text -> IO ()
main indata = do
  baseComp  <- executeParser parseComp indata
  netHub    <- atomically newTChan
  machines' <- forM [0 :: Int .. 49] $ \i -> do
    ch <- newTChanIO
    atomically $ writeTChan ch i
    -- "continuously trying to receive packets"
    -- let's just say that's 10 failed reads with no write or successful read
    -- which when summed would total 500, if everyone has failed to read 10
    -- times, then, it's probably idle
    -- this is probably not correct, but I'm not sure what is
    -- also don't know whether the computers might send something after X
    -- number of failed reads or if they will always continue asking
    -- they probably do react to failed reads since otherwise there would be
    -- no need for the -1 default value, they could just block instead
    activity   <- newTVarIO (0 :: Int)
    let recv = do
          msg <- atomically $ tryReadTChan ch
          case msg of
            Nothing -> do
              atomically $ modifyTVar activity (min 10 . succ)
              yield
              pure (-1)
            Just m -> atomically (writeTVar activity 0) >> pure m
    packagebuf <- newTVarIO []
    let send msg = do
          atomically $ writeTVar activity 0
          cur <- readTVarIO packagebuf
          case cur of
            [x, t] -> do
              atomically $ writeTChan netHub (t, x, msg)
              atomically $ writeTVar packagebuf []
            other -> atomically $ writeTVar packagebuf (msg : other)
    forkIO $ runM send recv baseComp
    pure (ch, activity)
  let machines = V.fromList $ fst <$> machines'
  let activity = V.fromList $ snd <$> machines'
  let loop m = m >> loop m
  -- NAT
  nat         <- newTChanIO
  natMemory   <- newTVarIO (0, 0)
  natPrevious <- newTVarIO Nothing
  forkIO $ loop $ do
    mx <- atomically $ tryReadTChan nat
    case mx of
      Just (x, y) -> atomically $ writeTVar natMemory (x, y)
      Nothing     -> pure ()
    total <- atomically $ sum <$> mapM readTVar activity
    when (total == 500) $ do
      (x, y) <- readTVarIO natMemory
      py     <- readTVarIO natPrevious
      when (py == Just y) $ atomically $ writeTChan netHub (254, x, y)
      atomically $ writeTVar natPrevious (Just y)
      atomically $ writeTChan netHub (0, x, y)
      let reset i = writeTVar (activity V.! i) 0
      atomically $ mapM_ reset [1 .. 49]
    yield
  -- network hub
  firstMsg <- newTVarIO True
  loop $ do
    (target, x, y) <- atomically $ readTChan netHub
    let t = machines V.! target
    case target of
      255 -> do
        whenM (readTVarIO firstMsg) $ do
          atomically $ writeTVar firstMsg False
          putText $ "first packet received by NAT, y=" <> show y
        atomically $ writeTChan nat (x, y)
      254 ->
        putText ("received shutdown signal with y=" <> show y) >> exitSuccess
      _ -> atomically $ writeTChan t x >> writeTChan t y
