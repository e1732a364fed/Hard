{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Hsimple.Map.Socks5
  ( Socks5Map (..),
    Socks5Config (..),
    AuthMethod (..),
  )
where

import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
import Network.Hsimple.Map
import Network.Hsimple.Map.Network
import Network.Hsimple.Map.Types
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB

-- | SOCKS5 authentication methods
data AuthMethod = NoAuth | UserPass BS.ByteString BS.ByteString
  deriving (Show, Eq)

-- | SOCKS5 configuration
data Socks5Config = Socks5Config
  { authMethod :: AuthMethod,
    -- | Allowed SOCKS commands (CONNECT=1, BIND=2, UDP=3)
    allowedCommands :: [Word8]
  }

-- | SOCKS5 proxy implementation
data Socks5Map = Socks5Map
  { config :: Socks5Config
  }

instance Map Socks5Map where
  maps cid behavior params = case behavior of
    DECODE -> handleClientConnection cid params
    ENCODE -> error "SOCKS5 encode not implemented"
    _ -> error "Invalid behavior for SOCKS5"

-- | Handle initial client connection
handleClientConnection :: (MonadIO io) => CID -> MapParams -> io MapResult
handleClientConnection cid MapParams {..} = do
  case baseConn of
    Conn sock -> do
      -- Read auth methods
      let handleAuth = case preReadBuf of
            Just buf -> handleAuthMethods cid sock buf
            Nothing -> return $ Left "No initial data"

      authResult <- handleAuth
      case authResult of
        Left err -> return $ mkErrorResult err
        Right () -> handleRequest cid sock
    _ -> return $ mkErrorResult "Invalid connection type"

-- | Handle authentication methods negotiation
handleAuthMethods :: (MonadIO io) => CID -> NS.Socket -> BS.ByteString -> io (Either String ())
handleAuthMethods cid sock buf = do
  let ver = BS.head buf
  if ver /= 0x05
    then return $ Left "Invalid SOCKS version"
    else do
      -- Select auth method (currently only NO AUTH)
      let response = BS.concat $ LBS.toChunks $ runPut $ do
            putWord8 0x05 -- Version
            putWord8 0x00 -- NO AUTH

      -- Send response
      sendAll sock response
      return $ Right ()

-- | Handle SOCKS request
handleRequest :: (MonadIO io) => CID -> NS.Socket -> io MapResult
handleRequest cid sock = do
  -- Read request
  reqBuf <- recv sock 4 -- Ver + Cmd + RSV + AddrType
  case BS.length reqBuf of
    4 -> do
      let ver = BS.head reqBuf
          cmd = BS.index reqBuf 1
          atyp = BS.index reqBuf 3

      if ver /= 0x05
        then return $ mkErrorResult "Invalid SOCKS version"
        else do
          addr <- readAddress sock atyp
          case addr of
            Left err -> return $ mkErrorResult err
            Right targetAddr -> do
              -- Handle CONNECT command
              if cmd == 0x01
                then
                  return $
                    MapResult
                      { resultAddr = Just targetAddr,
                        resultBuf = Nothing,
                        resultConn = Conn sock,
                        resultData = Nothing,
                        resultError = Nothing,
                        resultGlobalData = Nothing,
                        resultNewId = Nothing,
                        resultNoTimeout = False,
                        resultShutdownRx = Nothing
                      }
                else return $ mkErrorResult "Unsupported command"
    _ -> return $ mkErrorResult "Invalid request"

-- | Read target address based on address type
readAddress :: (MonadIO io) => NS.Socket -> Word8 -> io (Either String Addr)
readAddress sock atyp = case atyp of
  0x01 -> do
    -- IPv4
    addrBuf <- recv sock 6 -- 4 bytes IPv4 + 2 bytes port
    let ip =
          show $
            NS.SockAddrInet
              (fromIntegral $ runGet getWord16be $ LBS.fromStrict (BS.drop 4 addrBuf))
              (fromIntegral $ runGet getWord32host $ LBS.fromStrict (BS.take 4 addrBuf))
    return $ Right $ Addr ip (fromIntegral $ runGet getWord16be $ LBS.fromStrict (BS.drop 4 addrBuf))
  0x03 -> do
    -- Domain name
    lenBuf <- recv sock 1
    let len = BS.head lenBuf
    domainAndPort <- recv sock (fromIntegral len + 2)
    let domain = BS.take (fromIntegral len) domainAndPort
        port = runGet getWord16be $ LBS.fromStrict (BS.drop (fromIntegral len) domainAndPort)
    return $ Right $ Addr (show domain) (fromIntegral port)
  0x04 -> return $ Left "IPv6 not supported"
  _ -> return $ Left "Invalid address type"

-- Helper functions
mkErrorResult :: String -> MapResult
mkErrorResult err =
  MapResult
    { resultAddr = Nothing,
      resultBuf = Nothing,
      resultConn = None,
      resultData = Nothing,
      resultError = Just err,
      resultGlobalData = Nothing,
      resultNewId = Nothing,
      resultNoTimeout = False,
      resultShutdownRx = Nothing
    }