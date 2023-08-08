{-# LANGUAGE LambdaCase #-}
module StackFunge where

import Fungeoid

push :: Monad m => a -> Morell m g h p op [a] ()
push a = modState (a:)

pop :: Monad m => Morell m g h p op [a] a
pop = getState >>= \case
  (x:xs) -> putState xs >> return x
  [] -> error "Not enough values to pop from stack."

unary :: Monad m => (a -> a) -> Morell m g h p op [a] ()
unary f = do
  x <- pop
  push (f x)

binary :: Monad m => (a -> a -> a) -> Morell m g h p op [a] ()
binary f = do
  x <- pop
  y <- pop
  push (f x y)
