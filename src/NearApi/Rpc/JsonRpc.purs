module NearApi.Rpc.JsonRpc where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)

-- Overall envelope of JSON-RPC request
type JsonRpcRequest = 
    { jsonrpc :: String 
    , id :: String
    , method :: String
    , params :: Record 
        ( request_type :: String
        , finality :: String
        )
    }

-- The overall envelope of JSON-RPC response
type JsonRpcResponse = 
    { jsonrpc :: String
    , id :: String
    }

-- If there is an error at the JSON-RPC level
type JsonRpcResponseError =
    { error :: 
        { name :: String
        , cause :: 
            { name :: String
            , info :: 
                { error_message :: String 
                }
            }
        , code :: Int
        , message :: String
        , data :: String
        }
    }

-- If the JSON-RPC call is succesfully handled
type JsonRpcResponseResult =
    { result :: 
        { nonce :: Maybe Number
        , block_height :: Number
        , block_hash :: String
        }
    }

-- If the succesfully handled call throws an error
type JsonRpcResponseResultError = 
    { result ::  
        { error :: String
        , logs :: List String 
        }
    }