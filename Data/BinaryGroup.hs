module BinaryGroup where

import Data.Group

data BinaryGroup = E | One

instance Semigroup BinaryGroup where
  One <> E = One
  E <> One = One
  _ <> _ = E

instance Monoid BinaryGroup where
  mempty = E

instance Group BinaryGroup where
  invert E = One
  invert One = E
