{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Haxl.Core
import           System.Environment     (getEnv)
import           Web.Scotty

import           Models.Weather
import           Views.Index            (index)

foldA :: Foldable f => f [Weather] -> [Weather]
foldA = foldl mappend []

main :: IO ()
main = do
  myEnv <- initEnv (stateSet WeatherDataState stateEmpty) ()
  port <- read <$> getEnv "PORT"

  scotty port $
    get "/" $ do
      weathers <- liftIO $ runHaxl myEnv $
        (\a b -> [a, b])
        <$> getWeather "Helsinki, FI"
        <*> getWeather "Munich, DE"
      index weathers
