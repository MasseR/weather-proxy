{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language DeriveGeneric #-}
{-# Language RecordWildCards #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Types where

import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Aeson
import GHC.Generics (Generic)
import Servant

data Position = Position { positionLatitude :: Double
                         , positionLongitude :: Double }
              deriving (Show, Generic)

data Nominatim = Nominatim { nominatimPosition :: Position
                           , nominatimDisplay :: Text }
               deriving (Show, Generic)

data Environment = Environment {
    environmentDarkskyApiKey :: DSKey
} deriving (Show)

newtype Weather = Weather Value deriving (ToJSON, FromJSON, Show)

newtype OSMNominatim = OSMNominatim Nominatim deriving Show


newtype City = City Text deriving (Show, FromHttpApiData)
newtype Country = Country Text deriving (Show, FromHttpApiData)
newtype DSKey = DSKey String deriving (Show)

makeFields ''Position
makeFields ''Nominatim
makeFields ''Environment

makePrisms ''City
makePrisms ''Country
makePrisms ''DSKey
makePrisms ''OSMNominatim

instance FromJSON OSMNominatim where
    parseJSON (Object v) = do
        pos <- Position <$> (read <$> v .: "lat")
                        <*> (read <$> v .: "lon")
        nom <- Nominatim <$> pure pos <*> v .: "display_name"
        return $ OSMNominatim nom


instance ToJSON Position where
    toJSON Position{..} = object [ "latitude" .= positionLatitude
                                 , "longitude" .= positionLongitude ]

instance ToJSON Nominatim where
    toJSON Nominatim{..} = object [ "position" .= toJSON nominatimPosition
                                  , "display" .= nominatimDisplay ]

