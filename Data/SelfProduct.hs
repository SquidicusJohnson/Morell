module SelfProduct (
  SelfProduct (SProduct)
, unproduct
) where

import Data.Group
import Data.Monoid
import Data.Function
import Data.Maybe

import Commutative

data SelfProduct g = SProduct [g]
unproduct (SProduct xs) = xs

instance Show g => Show (SelfProduct g) where
  show = show . unproduct

instance Semigroup g => Semigroup (SelfProduct g) where
  (<>) = ((( SProduct . map fromJust . takeWhile isJust )).)  .  (zipWith (<>)) `on` ( (++ repeat Nothing) . map Just . unproduct )

instance Monoid g => Monoid (SelfProduct g) where
  mempty = SProduct $ [mempty]

instance Group g => Group (SelfProduct g) where
  invert = SProduct . map invert . unproduct

instance Commutative g => Commutative (SelfProduct g)
