{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Network.Hard.Map.Types where

import Data.ByteString qualified as BS
import GHC.Generics
import Network.Socket qualified as NS

-- | Connection ID for tracking
newtype CID = CID String
  deriving (Show, Eq)

-- | Proxy behavior type
data ProxyBehavior = UNSPECIFIED | ENCODE | DECODE
  deriving (Show, Eq)

-- | Network stream type
data Stream
  = Conn NS.Socket
  | Generator (IO Stream)
  | None

instance Show Stream where
  show (Conn sock) = "Conn <socket>"
  show (Generator _) = "Generator <IO Stream>"
  show None = "None"

-- | Network address type
data Addr = Addr
  { host :: String,
    port :: Int
  }
  deriving (Show, Eq, Generic)

-- | Map parameters
data MapParams = MapParams
  { targetAddr :: Maybe Addr,
    preReadBuf :: Maybe BS.ByteString,
    baseConn :: Stream,
    inputData :: [Maybe Data],
    globalData :: Maybe GlobalData,
    shutdownRx :: Maybe (IO ())
  }

-- | Map result
data MapResult = MapResult
  { resultAddr :: Maybe Addr,
    resultBuf :: Maybe BS.ByteString,
    resultConn :: Stream,
    resultData :: Maybe Data,
    resultError :: Maybe String,
    resultGlobalData :: Maybe GlobalData,
    resultNewId :: Maybe CID,
    resultNoTimeout :: Bool,
    resultShutdownRx :: Maybe (IO ())
  }

-- | Existential type for arbitrary data
data Data = forall a. (Show a) => Data a

-- | Global data type
data GlobalData = GlobalData
  { gdTag :: String
  }
  deriving (Show)