{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CalorieCalculator where

import Task (Task (..), enter, select, view, (>>?))
import Visualize (visualizeTask)
import "tophat" Prelude

data Gender = Male | Female

data ActivityLevel
  = Sedentary
  | Low
  | Active
  | VeryActive

type Height = Int

type Weight = Int

type Age = Int

-- Calculate how many calories you can eat per day. Based on:
-- https://www.ncbi.nlm.nih.gov/books/NBK278991/table/diet-treatment-obes.table12est/
main :: IO ()
main = visualizeTask calculateCaloriesTask

calculateCaloriesTask :: Task h Text
calculateCaloriesTask =
  introduction >>? \_ -> do
    (_, height) <- promptHeight
    (_, weight) <- promptWeight
    (_, age) <- promptAge
    (_, gender) <- promptGender
    (_, activityLevel) <- promptActivityLevel
    let calories = calculateCalories gender activityLevel height weight age
    view
      ( "Your resting metabolic rate is: "
          <> display calories
          <> " calories per day."
      )

introduction :: Task h Text
introduction =
  view
    <| unlines
      [ "This tool estimates your resting metabolic rate, i.e. the number of ",
        "calories you have to consume per day to maintain your weight.",
        "Press \"Continue\" to start"
      ]

promptGender :: Task h (Text, Gender)
promptGender =
  view "Select your gender:"
    >< select
      [ "Male" ~> Done Male,
        "Female" ~> Done Female
      ]

promptHeight :: Task h (Text, Height)
promptHeight = view "Enter your height in cm:" >< enter

promptWeight :: Task h (Text, Weight)
promptWeight = view "Enter your weight in kg:" >< enter

promptAge :: Task h (Text, Age)
promptAge = view "Enter your age:" >< enter

promptActivityLevel :: Task h (Text, ActivityLevel)
promptActivityLevel =
  view "What is your activity level?"
    >< select
      [ "Sedentary" ~> Done Sedentary,
        "Low active" ~> Done Low,
        "Active" ~> Done Active,
        "Very Active" ~> Done VeryActive
      ]

calculateCalories :: Gender -> ActivityLevel -> Height -> Weight -> Age -> Int
calculateCalories gender al h w age =
  let baseFormula = (9.99 * fromIntegral w) + (6.25 * fromIntegral h) - (4.92 * fromIntegral age)
      genderOffset = case gender of
        Male -> 5
        Female -> -161
      activityModifier = getActivityModifier gender al
   in round <| (baseFormula + genderOffset) * activityModifier

getActivityModifier :: Gender -> ActivityLevel -> Double
getActivityModifier gender al =
  case gender of
    Male -> case al of
      Sedentary -> 1
      Low -> 1.11
      Active -> 1.25
      VeryActive -> 1.48
    Female -> case al of
      Sedentary -> 1
      Low -> 1.12
      Active -> 1.27
      VeryActive -> 1.45
