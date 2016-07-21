{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Weather where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Typeable
import           GHC.Generics
import           Haxl.Core
import           Network.HTTP.Simple

import           Models.TH

data YahooWeatherLocation = YahooWeatherLocation
  { city    :: String
  , country :: String
  , region  :: String
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherLocation

data YahooWeatherWind = YahooWeatherWind
  { chill     :: String
  , direction :: String
  , speed     :: String
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherWind

data YahooWeatherAstronomy = YahooWeatherAstronomy
  { sunrise :: String
  , sunset  :: String
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherAstronomy

data YahooWeatherForecast = YahooWeatherForecast
  { forecastday  :: String
  , forecastdate :: String
  , forecasthigh :: String
  , forecastlow  :: String
  , forecasttext :: String
  } deriving (Generic, Show)
instance FromJSON YahooWeatherForecast where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 8
    }
makeLessStupidLenses ''YahooWeatherForecast

data YahooWeatherAtmosphere = YahooWeatherAtmosphere
  { humidity   :: String
  , pressure   :: String
  , rising     :: String
  , visibility :: String
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherAtmosphere

data YahooWeatherCondition = YahooWeatherCondition
  { conditiontemp :: String
  , conditiontext :: String
  } deriving (Generic, Show)
instance FromJSON YahooWeatherCondition where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 9
    }
makeLessStupidLenses ''YahooWeatherCondition

data YahooWeatherItem = YahooWeatherItem
  { title     :: String
  , condition :: YahooWeatherCondition
  , forecast  :: [YahooWeatherForecast]
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherItem

data YahooWeatherChannel = YahooWeatherChannel
  { astronomy     :: YahooWeatherAstronomy
  , atmosphere    :: YahooWeatherAtmosphere
  , item          :: YahooWeatherItem
  , lastBuildDate :: String
  , link          :: String
  , location      :: YahooWeatherLocation
  , wind          :: YahooWeatherWind
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherChannel

data YahooWeatherQueryResults = YahooWeatherQueryResults
  { channel :: YahooWeatherChannel
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherQueryResults

data YahooWeatherQuery = YahooWeatherQuery
  { count   :: Int
  , created :: String
  , lang    :: String
  , results :: YahooWeatherQueryResults
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeatherQuery

data YahooWeather = YahooWeather
  { query :: YahooWeatherQuery
  } deriving (Generic, Show, FromJSON)
makeLessStupidLenses ''YahooWeather

type Haxl a = GenHaxl () a

type WeatherId = String
type Weather = YahooWeather

data WeatherRequest a where
  FetchWeather :: WeatherId -> WeatherRequest Weather

deriving instance Show (WeatherRequest a)
deriving instance Typeable WeatherRequest
deriving instance Eq (WeatherRequest a)
instance Show1 WeatherRequest where show1 = show
instance Hashable (WeatherRequest a) where
  hashWithSalt salt (FetchWeather x) =
    salt `hashWithSalt` (0 :: Int) `hashWithSalt` x

getWeather :: WeatherId -> Haxl Weather
getWeather = dataFetch . FetchWeather

instance StateKey WeatherRequest where
  data State WeatherRequest = WeatherDataState

instance DataSourceName WeatherRequest where
  dataSourceName _ = "WeatherDataSource"

instance DataSource a WeatherRequest where
  fetch WeatherDataState _flags _userEnv blockedFetches =
    SyncFetch $ fetchWeather `mapM_` blockedFetches
buildYahooWeatherUrl :: String -> String
buildYahooWeatherUrl x
  = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22"
  ++ x
  ++ "%22)%20and%20u=%27c%27&format=json"

fetchWeather :: BlockedFetch WeatherRequest -> IO ()
fetchWeather (BlockedFetch (FetchWeather x) y) = do
  request <- parseRequest $ buildYahooWeatherUrl x
  weather <- getResponseBody <$> httpJSON request
  putSuccess y weather
