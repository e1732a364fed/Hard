{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hard.Map
  ( Map (..),
    MapBox (..),
  )
where

import Control.Monad.IO.Class
import Hard.Map.Types

-- | The Map typeclass represents a stream mapping function
class Map m where
  -- | Maps a stream with given parameters
  maps ::
    (MonadIO io) =>
    -- | Connection ID
    CID ->
    -- | ENCODE or DECODE
    ProxyBehavior ->
    -- | Input parameters
    MapParams ->
    -- | Result of mapping
    io MapResult

-- | Existential type for Map instances
data MapBox = forall m. (Map m) => MapBox m