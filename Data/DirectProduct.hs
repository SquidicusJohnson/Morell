module DirectProduct where

import Data.Group
import Data.Monoid

data DirectProduct g h = Cons g h

instance (Semigroup g, Semigroup h) => Semigroup (DirectProduct g h) where
  Cons g h <> Cons i l = Cons (g <> i) (i <> l)

instance (Monoid g, Monoid h) => Monoid (DirectProduct g h) where
  mempty = Cons mempty mempty

instance (Group g, Group h) => Group (DirectProduct g h) where
  invert (Cons g h) = Cons (invert g) (invert h)
