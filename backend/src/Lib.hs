{-# LANGUAGE DataKinds       #-}
  {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE TypeOperators   #-}
module Lib
  ( startApp
  , app
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


type API = "images" :> Raw 
    

{-type API =
        "users" :> Get '[JSON] [User]
  :<|>  "bigbrains" :> Get '[JSON] [BigBrains]-}

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = images

users :: [User]
users = [ User 1 "Isaac" "Newton"
  , User 2 "Albert" "Einstein"
        ]

images:: Server API 
images = serveDirectoryWebApp "n-puzzle-images"

{-admin :: [Admin]
admin = [ Admin 1 "Anna" "Cario"
  , Admin 2 "Lazare" "Rossillon"
        ]-}
