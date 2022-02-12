{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Task (enter, view, (>>?))
import Task.Syntax (Task)
import Visualize (visualizeTask)
import "tophat" Prelude hiding (guard, repeat)

type Address = Text

type Date = Int

type Amount = Int

main :: IO ()
main = visualizeTask tax

tax :: Task h Text
tax =
  let today :: Date
      today = 100

      provideDocuments :: Task h (Amount, Date)
      provideDocuments = enter >< enter

      companyConfirm :: Task h Bool
      companyConfirm = enter

      officerApprove :: Date -> Date -> Bool -> Task h Bool
      officerApprove invoiceDate date confirmed =
        view (date - invoiceDate < 365 && confirmed)
   in (provideDocuments >< companyConfirm)
        >>? \((invoiceAmount, invoiceDate), confirmed) ->
          officerApprove invoiceDate today confirmed
            >>? \approved ->
              let subsidyAmount =
                    if approved
                      then min 600 (invoiceAmount `div` 10)
                      else 0
               in view
                    <| unlines
                      [ "Subsidy amount: " ++ display subsidyAmount,
                        "Approved: " ++ display approved,
                        "Confirmed: " ++ display confirmed,
                        "Invoice date: " ++ display invoiceDate,
                        "Today: " ++ display today
                      ]
