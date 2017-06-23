{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
module API where

import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Lens
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types
import Control.Lens
import Network.Wreq hiding (Proxy)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Maybe

type API = "nominatim" :> Capture "city" City :> Capture "country" (Maybe Country) :> Get '[JSON] (Maybe Nominatim)
      :<|> "weather" :> Capture "city" City :> Get '[JSON] Weather

type MyHandler = ReaderT Environment (ExceptT ServantErr IO)

handlerToServant :: Environment -> MyHandler :~> Handler
handlerToServant env = Nat go
    where
        go :: MyHandler a -> Handler a
        go h = runReaderT h env

server :: Environment -> Server API
server env = enter (handlerToServant env) server'
    where
        server' = nominatim :<|> weather

weather :: City -> MyHandler Weather
weather city = do
    w <- runMaybeT $ do
        nom <- MaybeT $ nominatim city Nothing
        darksky (nom ^. position)
    maybe (throwError err404) return w

nominatim :: (MonadIO m) => City -> Maybe Country -> m (Maybe Nominatim)
nominatim city country = do
    let base = "http://nominatim.openstreetmap.org/search"
        opts = defaults & param "format" .~ ["json"]
                        & param "city" .~ [city ^. _City]
                        & param "country" .~ [country ^. _Just . _Country]
    r <- liftIO $ getWith opts base
    let noms = decode (r ^. responseBody) :: Maybe [OSMNominatim]
    return $ join $ fmap (listToMaybe . map (view _OSMNominatim))noms

darksky :: (HasDarkskyApiKey s DSKey, MonadCatch m, MonadReader s m, MonadIO m) => Position -> m Weather
darksky pos = do
    apikey <- view (darkskyApiKey . _DSKey)
    let url = base ++ apikey ++ "/" ++ coords
        base = "https://api.darksky.net/forecast/" 
        coords = lat ++ "," ++ lon
        lat = pos ^. (latitude . to show)
        lon = pos ^. (longitude . to show)
    r <- liftIO $ get url
    decode (r ^. responseBody)


weatherAPI :: Proxy API
weatherAPI = Proxy

app :: Environment -> Application
app env = serve weatherAPI (server env)
