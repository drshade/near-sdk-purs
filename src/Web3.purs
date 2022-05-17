module NearSdkPurs.Web3 where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either)
import Effect.Aff (Aff)

data KeyStore
    = BrowserLocalStorage
    | InMemory
    | UnencryptedFileSystem String
    | Merge KeyStore KeyStore

type NetworkConfiguration =
    { networkId     :: String
    , nodeUrl       :: String
    , walletUrl     :: String
    , helperUrl     :: String
    , explorerUrl   :: String
    }

mainnet :: NetworkConfiguration
mainnet = 
    { networkId     : "mainnet"
    , nodeUrl       : "https://rpc.mainnet.near.org"
    , walletUrl     : "https://wallet.mainnet.near.org"
    , helperUrl     : "https://helper.mainnet.near.org"
    , explorerUrl   : "https://explorer.mainnet.near.org"
    }

testnet :: NetworkConfiguration
testnet = 
    { networkId     : "testnet"
    , nodeUrl       : "https://rpc.testnet.near.org"
    , walletUrl     : "https://wallet.testnet.near.org"
    , helperUrl     : "https://helper.testnet.near.org"
    , explorerUrl   : "https://explorer.testnet.near.org"
    }

-- Enumerate all the different error types
data Web3Error
    = GeneralError String       -- Catchall for now

type Web3 a = ExceptT Web3Error Aff a

runWeb3 :: forall a. Web3 a -> Aff (Either Web3Error a)
runWeb3 = runExceptT