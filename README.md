# Hsimple

A proxy library written in Haskell, ported from the Rust version.

## Features

- Modular proxy protocol implementation
- Support for various protocols (SOCKS5, HTTP, Trojan)
- Composable proxy chains using Map pattern
- Asynchronous IO

## Architecture

The core concepts are:

- Map: A typeclass representing stream transformations
- ProxyBehavior: ENCODE/DECODE for outbound/inbound proxying
- Fold: Combining multiple Maps into proxy chains

## Building

```bash
cabal build
```

## Usage

```haskell
import Network.Hsimple.Map
import Network.Hsimple.Map.Socks5

-- Create a SOCKS5 proxy
proxy = Socks5Map {
  -- config
}

-- Use it in a proxy chain
chain = [
  MapBox proxy,
  -- other maps
]

-- Run the proxy
result <- fold FoldParams {
  fpCid = CID "conn-1",
  fpBehavior = DECODE,
  fpMaps = chain,
  -- other params
}
```

## License

BSD 3-Clause 