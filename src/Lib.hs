{-# LANGUAGE ExistentialQuantification #-}

module Lib
  ( runLoop,
    printLn
  )
where

import System.IO.Unsafe

data SideEffect a = Pure a | Effect (IO a) | forall c. Bind (SideEffect c) (c -> SideEffect a)

instance Functor SideEffect where
    fmap f (Pure a) = Pure $ f a
    fmap f (Effect ioa) = Effect $ fmap f ioa
    fmap f (Bind fc binding) = Bind fc $ \c -> let fa = binding c in fmap f fa

instance Applicative SideEffect where
    pure = Pure
    (Pure f) <*> fa = fmap f fa
    e@(Effect _) <*> fa = Bind e (`fmap` fa)
    (Bind fc binding) <*> fa = Bind fc (\c -> let ff = binding c in ff <*> fa)

instance Monad SideEffect where
    (Pure x) >>= f = f x
    e@(Effect _) >>= f = Bind e f
    (Bind fc binding) >>= f = Bind fc (\c -> let fa = binding c in fa >>= f)

runLoop :: SideEffect a -> IO a
runLoop (Pure x) = pure x
runLoop (Effect ioa) = ioa
runLoop (Bind fc f) = runLoop fc >>= runLoop . f

printLn :: String -> SideEffect ()
printLn str = Effect $ putStrLn str

