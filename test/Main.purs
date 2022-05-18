module Test.Main where

import Prelude

import Affjax (Error, Response, printError)
import Affjax as AX
import Affjax.Node as AN
import Affjax.RequestBody (json)
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), hush)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes, (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign.Object (toUnfoldable)


type JsonRpcReq a
    = Record ( jsonrpc :: String 
             , id :: String
             , method :: String
             , params :: Record ( request_type :: String
                                , finality :: String
                                | a 
                                )
             )

type ViewAccessKeyReq 
    = ( account_id :: String
      , public_key :: String
      )

type Cause 
    = ( name :: String
      , info :: Record 
            ( error_message :: String 
            )
      )

-- { 
--   "name":"REQUEST_VALIDATION_ERROR",
--   "cause": {
--     "name":"PARSE_ERROR",
--     "info": {
--       "error_message": "Failed parsing args: unknown variant `vie_access_key`, expected one of `view_account`, `view_code`, `view_state`, `view_access_key`, `view_access_key_list`, `call_function`"
--     }
--   },
--   "code": -32700,
--   "message": "Parse error",
--   "data": "Failed parsing args: unknown variant `vie_access_key`, expected one of `view_account`, `view_code`, `view_state`, `view_access_key`, `view_access_key_list`, `call_function`"
-- }
type ErrorRes
    = Record ( name :: String
             , cause :: Maybe (Record Cause)
             , code :: Int
             , message :: String
             , data :: String
             )

type JsonRpcRes a
    = Record ( jsonrpc :: String
             , id :: String
             , error :: Maybe ErrorRes
             , result :: Record 
                    ( nonce :: Maybe Number
                    , block_height :: Number
                    , block_hash :: String
                    | a
                    )
             )

type ErrorResultRes = 
    ( error :: String
    , logs :: List String 
    )

type FunctionCallPermission
    = (Record ( allowance :: String
              , method_names :: Array String
              , receiver_id :: String
              )
      )

data IndividualPermission
    = FullAccess
    | FunctionCall FunctionCallPermission

derive instance genericIndividualPermission :: Generic IndividualPermission _
instance showIndividualPermission :: Show IndividualPermission where show = genericShow

-- valid permission values from rpc are:
-- "FullAccess"
--
-- or something like this:
-- {
--   "FunctionCall": {
--     "allowance":"250000000000000000000000",
--     "method_names":["name"],
--     "receiver_id":"contractname"
--   }
-- }
--
-- and maybe others similar to the above?
--
instance decodeJsonPermission :: DecodeJson Permission where
    decodeJson :: Json -> Either JsonDecodeError Permission
    decodeJson blob =

        -- getFunctionCallPerm :: Object Json -> Maybe FunctionCallPermission
        -- getFunctionCallPerm obj =


        -- Could be "FullAccess"
        if toString blob == Just "FullAccess" then
            Right $ Permission $ FullAccess : Nil
        else
            -- Probably an object, e.g.:
            -- { "FunctionCall" : ... }
            case toObject blob of
                Nothing -> Left $ UnexpectedValue blob
                Just obj -> 
                    let
                        x :: List (Tuple String Json)
                        x = toUnfoldable obj
                        parse :: String -> Json -> Maybe IndividualPermission
                        parse "FunctionCall" json = hush $ FunctionCall <$> decodeJson json
                        parse _ _ = Nothing
                    in Right $ Permission $ catMaybes $ foldr (\(Tuple k v) acc -> parse k v : acc) Nil $ x

newtype Permission = Permission (List IndividualPermission)

derive instance genericPermission :: Generic Permission _
instance showPermission :: Show Permission where show = genericShow

type ViewAccessKeyRes
    = ( permission :: Permission )

main :: Effect Unit
main = do
    _ <- launchAff_  $ do
        let payload :: JsonRpcReq ViewAccessKeyReq
            payload = 
                { jsonrpc : "2.0"
                , id : "dontcare"
                , method : "query"
                , params : 
                    { request_type : "view_access_key"
                    , finality : "final"
                    , account_id : "tomwells.testnet"
                    , public_key : "ed25519:6szh72LjabzfaDnwLMcjs2bJpm1C7XkYubNEBDXy1cZ3"
                    }
                }
            pljson :: Json
            pljson = encodeJson payload 
        x :: Either Error (Response Json) <- AN.post ResponseFormat.json "https://rpc.testnet.near.org" (Just (json pljson))
        case x of
            Left err -> log $ printError err
            Right { body : bodyjson } ->
                let error_result :: Either JsonDecodeError (JsonRpcRes ErrorResultRes)
                    error_result = decodeJson bodyjson
                    good_body :: Either JsonDecodeError (JsonRpcRes ViewAccessKeyRes)
                    good_body = decodeJson bodyjson
                in log $ (stringify bodyjson) <> "\n\n" <> (show error_result) <> "\n\n" <> (show good_body)
        pure unit
    pure unit