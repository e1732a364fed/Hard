-- {-# LANGUAGE ImportQualifiedPost #-}

-- {-# LANGUAGE RecordWildCards #-}

module Hard.Map.Network
  ( recv,
    sendAll,
    connect,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
-- import Hard.Map.Types
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

-- | Receive exactly n bytes from socket
recv :: (MonadIO io) => NS.Socket -> Int -> io BS.ByteString
recv sock n = liftIO $ go [] n
  where
    go acc 0 = return $ BS.concat $ reverse acc
    go acc remaining = do
      chunk <- NSB.recv sock (min remaining 4096)
      if BS.null chunk
        then return $ BS.concat $ reverse acc
        else go (chunk : acc) (remaining - BS.length chunk)

-- | Send all bytes to socket
sendAll :: (MonadIO io) => NS.Socket -> BS.ByteString -> io ()
sendAll sock = liftIO . NSB.sendAll sock

-- | Connect to remote address
connect :: (MonadIO io) => NS.Socket -> NS.SockAddr -> io ()
connect sock addr = liftIO $ NS.connect sock addr