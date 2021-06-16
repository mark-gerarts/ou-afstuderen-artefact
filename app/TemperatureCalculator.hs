{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TemperatureCalculator where

import Task (Task (..), enter, select, view, (>>?))
import Visualize (visualizeTask)
import "tophat" Prelude

data Conversion = CelciusToFahrenheit | FahrenheitToCelcius

main :: IO ()
main = visualizeTask tempuratureCalculator

tempuratureCalculator :: Task h Text
tempuratureCalculator = do
  conversion <-
    select
      [ "Celcius to Fahrenheit" ~> Done CelciusToFahrenheit,
        "Fahrenheit to Celcius" ~> Done FahrenheitToCelcius
      ]
  convert conversion

convert :: Conversion -> Task h Text
convert conversionType =
  (enter :: Task h Int)
    >>? view << display << calculate conversionType

calculate :: Conversion -> Int -> Double
calculate CelciusToFahrenheit c = (fromIntegral c * 9 / 5) + 32
calculate FahrenheitToCelcius f = (fromIntegral f - 32) * 5 / 9
