{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Change where

import Task (Task (..), change, share, view, (<<-))
import Visualize (visualizeTask)
import "tophat" Prelude hiding (guard, repeat)

main :: IO ()
main = visualizeTask doubleShared

doubleShared :: (Reflect h) => Task h ((), Int)
doubleShared = do
  r <- share (0 :: Int)
  m <- share (0 :: Int)
  t2 r m >< t1 r m
  where
    t1 _ m = do
      x <- change m
      if x >= 10 then view (x * 2) else fail
    t2 r m = do
      y <- change r
      if y >= 5 then m <<- 12 else fail
