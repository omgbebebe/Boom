{-# LANGUAGE OverloadedStrings #-}
module Boom.Helpers where

import Control.Concurrent
import System.IO.Unsafe
import qualified Text.XML as X
import Text.XML.Cursor
import Data.Text as T
import Data.Text.Lazy as TL (fromStrict)
import qualified Data.List as DL
import Network.Simple.TCP
import System.INotify
import Control.Exception

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

mac_from_xml :: Text -> Text
mac_from_xml xml = mac
  where doc  = X.parseText_ X.def $ TL.fromStrict xml
        cursor = fromDocument doc
        macs = T.concat $ cursor
               $// element "interface"
               >=> attributeIs "type" "network"
               >=> child
               >=> element "source"
               >=> attributeIs "network" "default"
               >=> parent
               >=> child
               >=> element "mac"
               >=> attribute "address"
        mac = macs

wait_node_ip :: FilePath -> Text -> IO (Text)
wait_node_ip dhcpLeases mac = do
  inot <- initINotify
  res <- newEmptyMVar
  wd <- addWatch inot [Modify] dhcpLeases (readLeases res mac)
  putStrLn $ "waiting for " ++ (T.unpack mac)
  r <- takeMVar res
  removeWatch wd
  return (r)
  where readLeases m mac' _ = do
          c <- readFile dhcpLeases
          let ll = DL.filter (DL.isInfixOf $ T.unpack mac') $ DL.lines c
          case DL.length ll of
            0 -> return ()
            1 -> do
              let [_,_,ip,_,_] = DL.words $ ll !! 0
              putMVar m $ T.pack ip
            _ -> putMVar m "to many results"

wait_for_port :: Text -> Int -> IO ()
wait_for_port ip port = do
  m <- newEmptyMVar
  forkIO $ wait_for_port' m ip port
  takeMVar m

wait_for_port' :: MVar () -> Text -> Int -> IO ()
wait_for_port' m ip port = do
  res <- try (connect (T.unpack ip) (show port) $ 
              \(s, addr) -> return "connected") :: IO (Either SomeException String)
  case res of
    Right _ -> putMVar m ()
    Left e -> do
      putStrLn $ show e
      threadDelay (2 * 1000000)
      wait_for_port' m ip port

