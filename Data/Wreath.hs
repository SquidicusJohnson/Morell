
module Wreath (
  Wreath (Wreath)
) where

import Permute
import Action
import SelfProduct

import Data.Group
import Data.Monoid
import Data.Function (on)

data Wreath g h = Wreath g h

instance (Show g, Show h) => Show (Wreath g h) where
  show (Wreath g h) = show (g, h)

instance (Semigroup g, Semigroup h, Action g h) => Semigroup (Wreath g h) where
  (Wreath p g) <> (Wreath q h) = Wreath (p <> q) ( (act q g) <> h )

instance (Group g, Group h, Action g h) => Monoid (Wreath g h) where
  mempty = Wreath mempty mempty

instance (Group g, Group h, InverseAction g h) => Group (Wreath g h) where
  invert (Wreath p g) = Wreath (invert p) ( inverseAct p g )
