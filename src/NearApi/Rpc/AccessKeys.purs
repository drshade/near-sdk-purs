module NearApi.Rpc.AccessKeys where

import Prelude

import Data.Argonaut.Core (Json, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either(..), hush)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), catMaybes)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Foreign.Object (toUnfoldable)

type ViewAccessKeyRequest = 
    { account_id :: String
    , public_key :: String
    }

type ViewAccessKeyRes = 
    { permission :: Permission 
    }

newtype Permission = Permission (List IndividualPermission)

derive instance genericPermission :: Generic Permission _
instance showPermission :: Show Permission where show = genericShow

data IndividualPermission
    = FullAccess
    | FunctionCall FunctionCallPermission

derive instance genericIndividualPermission :: Generic IndividualPermission _
instance showIndividualPermission :: Show IndividualPermission where show = genericShow

type FunctionCallPermission
    = (Record ( allowance :: String
              , method_names :: Array String
              , receiver_id :: String
              )
      )

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
