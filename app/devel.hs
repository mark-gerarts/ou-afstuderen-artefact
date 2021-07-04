{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

import "tophat" Task (Label, Task, enter, select, update, view, (<?>), (>>?))
import Visualize (visualizeTaskDevel)
import "tophat" Prelude

-- This file is used for development purposes in combination with yesod-devel.
main :: IO ()
main = visualizeTaskDevel startCandyMachine

--Example tasks

enterTask :: Task h Bool
enterTask = enter

stepViewUpdate :: Task h Text
stepViewUpdate =
  update ("Hello!" :: Text) >< (update True >< update (1 :: Int)) >>? \(t, (b, i)) ->
    view
      <| unwords
        [ "The left value was \"",
          t,
          "\", the right value was",
          display (b, i)
        ]

oneStep' :: Task h Int
oneStep' =
  update 0 >>? \x ->
    view x

pick2 :: Task h Int
pick2 = view 1 <?> view 2

pick3' :: Task h Int
pick3' =
  select
    [ "B" ~> view 22,
      "A" ~> view 11
    ]

-- Multiplication-by-seven machine

multiplication :: Int -> Int -> Task h Int
multiplication x y = view (x * y)

multBySeven :: Int -> Task h Int
multBySeven x = multiplication x 7

multBySevenMachine :: Task h Int
multBySevenMachine =
  enter >>? \x ->
    multBySeven x

-- CandyMachine
candy :: HashMap Label (Task h (Text, Text))
candy =
  [ entry "Pure Chocolate" 8,
    entry "IO Chocolate" 7,
    entry "Sem Chocolate" 9
  ]
  where
    entry :: Text -> Int -> (Label, Task h (Text, Text))
    entry name price =
      (name, view "You need to pay:" >< (view price >>? \x -> payCandy x))

payCoin :: Int -> HashMap Label (Task h Int)
payCoin bill =
  [ coinSize 5,
    coinSize 2,
    coinSize 1
  ]
  where
    coinSize :: Int -> (Label, Task h Int)
    coinSize size = (display size, view (bill - size))

startCandyMachine :: (Task h (Text, (Text, Text)))
startCandyMachine = view "We offer you three chocolate bars. Pure Chocolate: It's all in the name. IO Chocolate: Chocolate with unpredictable side effects. Sem Chocolate: don't try to understand, just eat it!" >< select candy

payCandy :: Int -> Task h Text
payCandy bill =
  select (payCoin bill)
    >>? \billLeft ->
      case compare billLeft 0 of
        EQ -> dispenseCandy Fair
        LT -> dispenseCandy Evil
        GT -> payCandy billLeft

data CandyMachineMood = Fair | Evil

dispenseCandy :: CandyMachineMood -> Task h Text
dispenseCandy Fair = view "You have paid. Here is your candy. Enjoy it!"
dispenseCandy Evil = view "You have paid too much, fool! You don't get change, but here is your candy."
