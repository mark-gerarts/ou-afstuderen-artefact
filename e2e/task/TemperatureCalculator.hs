{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TemperatureCalculator where

import Task (enter, pick, view, (>>?))
import Task.Syntax (Task)
import Visualize (visualizeTask)
import "tophat" Prelude

data Conversion = CelciusToFahrenheit | FahrenheitToCelcius

main :: IO ()
main = visualizeTask tempuratureCalculator

tempuratureCalculator :: Task h Text
tempuratureCalculator =
  pick
    [ "Celcius to Fahrenheit" ~> convert CelciusToFahrenheit,
      "Fahrenheit to Celcius" ~> convert FahrenheitToCelcius
    ]

convert :: Conversion -> Task h Text
convert conversionType =
  (enter :: Task h Int)
    >>? view << display << calculate conversionType

calculate :: Conversion -> Int -> Double
calculate CelciusToFahrenheit c = (fromIntegral c * 9 / 5) + 32
calculate FahrenheitToCelcius f = (fromIntegral f - 32) * 5 / 9
