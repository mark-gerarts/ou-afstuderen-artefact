{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ChatSession where

import Task (Store (..), Task (..), enter, repeat, share, watch, (<<=), (>>*))
import Visualize (visualizeTask)
import "tophat" Prelude hiding (guard, repeat)

main :: IO ()
main = visualizeTask chatSession

chatSession :: Reflect h => Task h ((Text, ()), (Text, ()))
chatSession = do
  history <- share ""
  chat "Tim" history >< chat "Nico" history
  where
    chat :: Text -> Store h Text -> Task h (Text, ())
    chat name history =
      watch history >< repeat (enter >>* ["Send" ~> append history name])

    append :: Store h Text -> Text -> Text -> Task h ()
    append history name msg = do
      history <<= \h ->
        (if h == "" then h else h ++ "\n") ++ name ++ ": '" ++ msg ++ "'"
