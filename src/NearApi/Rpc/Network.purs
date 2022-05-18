module NearApi.Rpc.Network where

import Prelude

type NetworkConfig = 
    { rpc :: String 
    }

mainnet :: NetworkConfig
mainnet = { rpc : "https://rpc.mainnet.near.org" }

testnet :: NetworkConfig
testnet = { rpc : "https://rpc.testnet.near.org" }

betanet :: NetworkConfig
betanet = { rpc : "https://rpc.betanet.near.org" }

