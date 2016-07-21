{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Haxl.Core
import           System.Environment     (getEnv)
import           Web.Scotty

import           Models.Weather
import           Views.Index            (index)

main :: IO ()
main = do
  myEnv <- initEnv (stateSet WeatherDataState stateEmpty) ()
  port <- read <$> getEnv "PORT"

  scotty port $
    get "/" $ do
      weather <- liftIO $ runHaxl myEnv $ getWeather "Helsinki, FI"
      index weather
