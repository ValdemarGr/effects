module Lib
    ( someFunc
    , liftList
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

liftList :: a -> [a]
liftList x = [x]
