{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hard.Map.Fold
  ( FoldParams (..),
    FoldResult (..),
    fold,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Hard.Map
import Hard.Map.Types

-- | Parameters for folding Maps
data FoldParams = FoldParams
  { -- | Connection ID
    fpCid :: CID,
    -- | ENCODE or DECODE
    fpBehavior :: ProxyBehavior,
    -- | Initial state
    fpInitialState :: MapResult,
    -- | Maps to fold
    fpMaps :: [MapBox],
    -- | Chain tag
    fpChainTag :: String
  }

-- | Result of folding Maps
data FoldResult = FoldResult
  { frAddr :: Maybe Addr,
    frBuf :: Maybe BS.ByteString,
    frConn :: Stream,
    frData :: [Maybe Data],
    frError :: Maybe String,
    frId :: CID,
    frChainTag :: String,
    frLeftMaps :: [MapBox],
    frNoTimeout :: Bool,
    frShutdownRx :: Maybe (IO ())
  }

-- | Fold a list of Maps
fold :: (MonadIO io) => FoldParams -> io FoldResult
fold FoldParams {..} = loop fpMaps fpInitialState [] fpChainTag
  where
    loop remainingMaps lastResult outputs tag = case remainingMaps of
      [] -> return $ mkResult remainingMaps lastResult outputs tag
      (MapBox (m :: m) : rest) -> do
        let params =
              MapParams
                { targetAddr = resultAddr lastResult,
                  preReadBuf = resultBuf lastResult,
                  baseConn = resultConn lastResult,
                  inputData = outputs,
                  globalData = resultGlobalData lastResult,
                  shutdownRx = resultShutdownRx lastResult
                }

        result <-
          maps @m
            (maybe fpCid id (resultNewId lastResult))
            fpBehavior
            params

        let newTag =
              if null tag
                then maybe "" id (resultGlobalData result >>= Just . gdTag)
                else tag

        let newOutputs = outputs ++ [resultData result]

        if isStreamDone (resultConn result) || isJust (resultError result)
          then return $ mkResult rest result newOutputs newTag
          else loop rest result newOutputs newTag

    isStreamDone None = True
    isStreamDone (Generator _) = True
    isStreamDone _ = False

    mkResult maps result outputs tag =
      FoldResult
        { frAddr = resultAddr result,
          frBuf = resultBuf result,
          frConn = resultConn result,
          frData = outputs,
          frError = resultError result,
          frId = maybe fpCid id (resultNewId result),
          frChainTag = tag,
          frLeftMaps = maps,
          frNoTimeout = resultNoTimeout result,
          frShutdownRx = resultShutdownRx result
        }