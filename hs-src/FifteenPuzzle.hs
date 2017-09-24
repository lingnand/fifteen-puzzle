{-# LANGUAGE ForeignFunctionInterface #-}
module FifteenPuzzle
    ( runGame
    )
  where

import qualified FifteenPuzzle.Main as M

runGame :: IO ()
runGame = M.main

foreign export ccall runGame :: IO ()