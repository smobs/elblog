module WebAPI.Settings where
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (gShow)
import Servant.PureScript.Settings (SPSettings_(..))
import WebAPI (SPParams_(..))

settings :: SPSettings_ SPParams_
settings =  SPSettings_ { 
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ {
                      baseURL : "http://localhost:8080/"
                  }}
