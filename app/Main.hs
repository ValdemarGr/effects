module Main where

import Lib
import System.IO.Unsafe

program = printLn "hoooi!"

{-# NOINLINE main #-}
main :: IO ()
main = runLoop program
