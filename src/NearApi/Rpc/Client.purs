module NearApi.Rpc.Client where

import Prelude

import Affjax (Error, Response, printError) as Affjax
import Affjax.Node (post) as AffjaxNode
import Affjax.RequestBody (json) as RequestBody
import Affjax.ResponseFormat (json) as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import NearApi.Rpc.AccessKeys (ViewAccessKeyRes)
import NearApi.Rpc.JsonRpc (JsonRpcRequest, JsonRpcResponseError, JsonRpcResponseResultError, JsonRpcResponse)
import NearApi.Rpc.Network (NetworkConfig, mainnet, testnet)
import Record as Record

data ClientError
    = TransportError String
    | MethodError String
    | OtherError String

derive instance genericClientError :: Generic ClientError _
instance showClientError :: Show ClientError where show = genericShow

rpc_request network method method_params =
    let rpc =   { jsonrpc : "2.0"
                , id : "dontcare"
                , method : "query"
                , params : 
                    { request_type : method
                    , finality : "final"
                    }
                }
        merged_envelope = rpc { params = Record.union rpc.params method_params }
    in call network merged_envelope

view_access_keys' :: NetworkConfig -> Aff (Either ClientError { result :: ViewAccessKeyRes })
view_access_keys' network = 
    rpc_request network "view_access_key"
        { account_id : "tomwells.testnet"
        , public_key : "ed25519:6szh72LjabzfaDnwLMcjs2bJpm1C7XkYubNEBDXy1cZ3"
        }

view_access_keys :: Aff (Either ClientError { result :: ViewAccessKeyRes })
view_access_keys = 
    let rpc =   { jsonrpc : "2.0"
                , id : "dontcare"
                , method : "query"
                , params : 
                    { request_type : "view_access_key"
                    , finality : "final"
                    }
                }
        r =     { account_id : "tomwells.testnet"
                , public_key : "ed25519:6szh72LjabzfaDnwLMcjs2bJpm1C7XkYubNEBDXy1cZ3"
                }

        m = rpc { params = Record.union rpc.params r }
    in call testnet m

decideError :: forall a. DecodeJson a => Either Affjax.Error (Affjax.Response Json) -> Either ClientError a
decideError (Left err) = Left $ TransportError $ Affjax.printError err
decideError (Right { body : bodyjson }) =
    let error_result :: Either JsonDecodeError JsonRpcResponseError
        error_result = decodeJson bodyjson
        error_result' :: Either JsonDecodeError JsonRpcResponseResultError
        error_result' = decodeJson bodyjson
        final_result :: Either JsonDecodeError a
        final_result = decodeJson bodyjson
    in 
    case Tuple error_result error_result' of
        Tuple (Left _) (Left _) -> 
            case final_result of
                Left err -> Left $ OtherError $ show err
                Right b -> Right b
        Tuple (Right err) _ -> Left $ TransportError $ err.error.message
        Tuple _ (Right err) -> Left $ MethodError $ err.result.error

call :: forall req res. EncodeJson req => DecodeJson res => NetworkConfig -> req -> Aff (Either ClientError res)
call network req =
    let encodedRequestBody :: Json
        encodedRequestBody = encodeJson req
    in do
        rpcRes :: Either Affjax.Error (Affjax.Response Json) <- AffjaxNode.post ResponseFormat.json network.rpc $ Just $ RequestBody.json encodedRequestBody
        pure $ decideError rpcRes

