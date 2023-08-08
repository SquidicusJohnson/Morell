{-# LANGUAGE LambdaCase #-}
module StandardFunge where

import Data.Group

import Fungeoid
import Wreath
import SelfProduct
import Permute
import Wreath
import Action
import Commutative
import BinaryGroup

type StandardFunge programType opType s c = Morell IO (Wreath Permutation (SelfProduct BinaryGroup)) [Int] programType opType s c

instance Semigroup Int where
  (<>) =  (+)

instance Monoid Int where
  mempty = fromInteger 0

instance Group Int where
  invert = negate

instance Commutative Int

instance Group g => Action Permutation (SelfProduct g) where
  act p = SProduct . actOnMonoid p . unproduct

instance Group g => InverseAction Permutation (SelfProduct g) where
  inverseAct p = SProduct . inverseActOnMonoid p . unproduct

instance Action (Wreath Permutation (SelfProduct BinaryGroup) ) [Int]
  where act (Wreath axisPermutation axisInversions) = actOnMonoid axisPermutation . zipWith ($) (map (\case { E -> id ; One -> negate }) (unproduct axisInversions) )
